# Asynchronous Download & `task_q` Scheduler — Developer Guide

This document brings a new developer up to speed on the most complex part of the
Analyse Together application: the **asynchronous download subsystem**. It covers
the `task_q` scheduler, the `queue_manager.R` consumer process, and the
database-as-message-bus design that ties them together.

- Audience: developers new to the codebase.
- Scope: `funs/queue_fun.R`, `funs/download_fun.R`, `funs/scheduler_fun.R`,
  `scripts/queue_manager.R`, and the relevant parts of `global.R` and the
  `modules/get_data_button.R` module.

---

## 1. Why this exists (the core design idea)

Downloading sensor data from the Samen Meten / KNMI / LML APIs is **slow**
(minutes to hours for a whole municipality). Shiny is single-threaded per
session, so downloading inline would freeze the UI for every user.

The solution is a **producer/consumer system decoupled through the SQLite
database**:

- The **Shiny app** (producer) never downloads during browsing. It only *reads*
  cached data, and when data is missing it writes a **job document** ("data
  request") into the database.
- A **separate R process, `queue_manager.R`** (consumer), continuously polls the
  database for job documents, downloads the data in parallel using a pool of
  background R sessions (`task_q`), and writes the results back into the same
  database.

The database is both the **cache** and the **message bus**. There is no direct
socket/IPC between the two processes — they communicate only through rows in the
`meta` table.

```mermaid
flowchart LR
    subgraph app["Shiny process (global.R / server.R)"]
        GB["get_data_button module<br/>'Download external'"]
    end
    subgraph qm["queue_manager.R process (background)"]
        LOOP["main while(TRUE) loop"]
        TQ["task_q (R6)<br/>pool of 4 callr sessions"]
    end
    DB[("SQLite database.db<br/>meta / measurements / location / cache")]
    API["External APIs<br/>(samanapir: SamenMeten, KNMI, LML)"]

    GB -->|"create_data_request()<br/>writes data_req doc"| DB
    LOOP -->|"list_doc('data_req')<br/>poll for jobs"| DB
    LOOP --> TQ
    TQ -->|"dl_station()"| API
    API --> TQ
    TQ -->|"store measurements + locations"| DB
    app -->|"read cached data only"| DB
```

Both processes are started from `global.R` at container boot:

```r
system2("Rscript", db_script, wait = TRUE)    # container_data_prep.R  (seed DB)
system2("Rscript", qm_script, wait = FALSE)   # queue_manager.R        (background downloader)
```

> **Note on dead code.** `global.R` also contains a *commented-out* in-app
> `que <- task_q$new()`, and `server.R` has a `suspended = TRUE` observer calling
> `que$poll()`. These are **disabled**. In production, `task_q` runs **only**
> inside the `queue_manager.R` process. Do not be misled by the app-side
> references.

### 1.1 Design rationale (why it is built this way)

- **Separate OS process + database as the queue.** The application is deployed as
  **multiple containers**, so a shared database (rather than in-process state) was
  preferred as the coordination point. Running the downloader as its own process
  also makes the Shiny application **more stable**, and moving queue management out
  of the (single-threaded) Shiny process **reduces CPU load** inside the app so the
  UI stays responsive.
- **File lock = single queue manager.** With several containers potentially
  running the same code against the same database, the `filelock` ensures that
  **only one container actually manages/drains the queue** at a time. The others
  back off until the lock is free.
- **Concurrency of 4.** Chosen for a balance of **limited container size** and
  **API politeness**. Experience showed 4 to be a good mix of download speed while
  keeping both the container and the upstream APIs performing well.
- **Duplicate-station deferral.** Added specifically to fix a **data-corruption
  bug** that occurred when the same station was downloaded concurrently (see
  §4.5a).

---

## 2. The unit of work: a "data request" job document

A job is created by `create_data_request()` (`funs/queue_fun.R`) and stored via
`ATdatabase::add_doc()` in the `meta` table with `type = "data_req"`.

- Input: a vector of `kits` (station ids) plus start/end times.
- Stations are chunked into sub-requests of at most `max_requests` (100) stations
  each.
- Each chunk is stored as a document with ref `id<random>_<seq>`, e.g.
  `id0123456789_0001`. The document body is a data frame of
  `station, time_start, time_end`.

```mermaid
flowchart TB
    REQ["create_data_request(kits, t_start, t_end)"]
    REQ --> SPLIT["split into chunks of up to 100 stations"]
    SPLIT --> D1["meta doc<br/>type=data_req<br/>ref=id..._0001"]
    SPLIT --> D2["meta doc<br/>type=data_req<br/>ref=id..._0002"]
    SPLIT --> DN["..."]
```

Lifecycle of a job document in `meta`:

`data_req` (queued) → processed by queue_manager → removed and re-stored as
`data_req_done` (with timing stats).

---

## 3. `queue_manager.R` — the consumer loop

A single infinite `while(TRUE)` loop. Each iteration:

```mermaid
flowchart TB
    START(["loop iteration"]) --> LOCK{"acquire filelock<br/>(timeout 1000ms)"}
    LOCK -->|"failed"| SLEEP1["sleep 60-120s random"] --> START
    LOCK -->|"ok"| LIST["joblist = list_doc('data_req')"]
    LIST --> HASJOB{"any jobs?"}
    HASJOB -->|"no"| SCHED{"idle 12x?<br/>past 02:00 &<br/>not run today?"}
    SCHED -->|"yes"| RUNSCHED["run_scheduled()<br/>create new data_req docs"] --> SLEEP2
    SCHED -->|"no"| SLEEP2["sleep 30s"] --> START
    HASJOB -->|"yes"| TAKE["job_id = joblist[1]<br/>load its station list j"]
    TAKE --> NEWQ["que = task_q$new()  (4 workers)"]
    NEWQ --> PUSH["for each station: que$push(dl_station, args, id=station)"]
    PUSH --> DRAIN["drain loop:<br/>while(nrow(tasks) > 4) { que$pop(); sleep 5s }"]
    DRAIN --> WATCH{"watchdog:<br/>njobs stuck >10 checks?"}
    WATCH -->|"stalled"| BREAK["break, success=FALSE"]
    WATCH -->|"draining"| DRAIN
    DRAIN -->|"all done"| OK["rm(que); gc()"]
    BREAK --> OK
    OK --> DONE{"success?"}
    DONE -->|"yes"| MOVE["remove_doc(data_req)<br/>add_doc(data_req_done, +timing)"]
    DONE -->|"no"| RETRY["leave data_req in place -> retried next iteration"]
    MOVE --> START
    RETRY --> START
```

Key points:

- **One job document per iteration.** It takes `joblist[1]`, processes all its
  (up to 100) stations, then loops. Multiple job docs are handled sequentially
  across iterations.
- **A fresh `task_q` is created per job** and destroyed (`rm; gc()`) afterward —
  the 4 worker sessions are not kept alive between jobs.
- **`filelock`** (`data/lock`) guarantees only one downloader runs at a time even
  if several processes/containers point at the same DB. A process that cannot get
  the lock backs off 60–120 s. This is deliberate: the app is deployed as
  **multiple containers**, and only one of them should manage/drain the queue.
- **The `> 4` magic number** equals `concurrency` (4 idle worker rows always
  remain in the task table; see §4). So `nrow(tasks) > 4` means "real tasks still
  pending", and `njobs = nrow - 4`.
- **Watchdog.** Every 10th drain check (~every 50 s) it compares the pending
  count; if unchanged for more than 10 such checks it assumes a stall and breaks
  out (the job stays `data_req` and is retried later).
- **Scheduler.** When the queue has been empty for 12 polls (~6 min), and it is
  past the configured `02:00` and has not run today (`check_schedule`),
  `run_scheduled()` reads `prepped_data/schedule.csv` and enqueues fresh daily
  data requests for the listed municipalities/projects. See
  `funs/scheduler_fun.R`.

---

## 4. `task_q` — the parallel task scheduler (the subtle part)

`task_q` (`funs/queue_fun.R`) is an **R6 class** adapted from callr's official
"task queue" example (`?callr::r_session`). It manages a fixed pool of persistent
background R sessions and distributes tasks across them. The key to understanding
it is that **one tibble tracks both workers and tasks**.

### 4.1 The single state table

`private$tasks` is a tibble where **each row is either a worker slot or a task**:

| column        | meaning                                                             |
|---------------|--------------------------------------------------------------------|
| `id`          | task id (station id) or `.idle-N` for a worker slot                 |
| `seq`         | global push order (`.1`, `.2`, ...)                                 |
| `idle`        | **TRUE = permanent worker slot**, FALSE = real task                |
| `state`       | `waiting` -> `running` -> `ready` -> `done` (plus `duplicate`)      |
| `fun` / `args`| the function (`dl_station`) and its arguments                      |
| `worker`      | a `callr::r_session` object when one is attached, else `NULL`       |
| `result`      | the value read back from the worker                                 |

At construction, `start_workers(concurrency = 4)` inserts **4 permanent `idle`
rows**, each holding one `callr::r_session$new(wait = FALSE)` (a background R
process booting up). These 4 rows are never removed; they represent the worker
pool. Real tasks are always inserted **above** the idle rows
(`add_row(.before = first idle row)`), which matters for scheduling priority.

```
+- real tasks (idle = FALSE) ---------------------------+
| id=NL101  state=running  worker=W3   <- downloading   |
| id=NL202  state=waiting  worker=NULL <- queued        |
+- permanent worker slots (idle = TRUE) ----------------+
| id=.idle-1 state=ready   worker=W1   <- free          |
| id=.idle-2 state=ready   worker=W2   <- free          |
| id=.idle-3 state=waiting worker=NULL <- parked        |
| id=.idle-4 state=running worker=W4   (booting)        |
+-------------------------------------------------------+
```

### 4.2 The four public methods

- **`push(fun, args, id)`** — insert a task row above the idle rows, then call
  `schedule()`.
- **`poll(timeout)`** — ask the OS (`processx::poll`) which running worker
  connections have signalled completion, flip those rows `running -> ready`, then
  call `schedule()`. Returns ids of any `done` tasks.
- **`pop(timeout)`** — `poll()`, then remove the first `done` task row and return
  its `result` + `task_id`. Returns `NULL` if nothing is done.
- **`schedule()`** (private) — the engine; see §4.4.

### 4.3 State machine for a single task

```mermaid
stateDiagram-v2
    [*] --> waiting: push()
    waiting --> running: schedule() assigns a free worker<br/>and calls worker$call(fun,args)
    running --> ready: poll() sees the worker's<br/>connection signal completion
    ready --> done: schedule() reads result,<br/>returns worker to the pool
    done --> [*]: pop() removes the row

    waiting --> duplicate: push() detected same id<br/>already in the table
    duplicate --> waiting: schedule() sees the earlier<br/>same-id task has left the table
```

### 4.4 How `schedule()` moves workers

`schedule()` runs on every `push`/`poll`. Conceptually:

1. **Harvest finished work.** Find rows in state `ready` (a worker signalled
   done). Read each worker's result into `result`, detach the worker
   (`worker <- NULL`), and set the row to `done` (real task) or `waiting` (idle
   slot that just finished booting).
2. **Dispatch to free workers.** Take the just-freed `r_session` objects and
   assign them to the **first** `length(ready)` rows that are in state `waiting`.
   Because real tasks sit *above* idle rows, waiting tasks are picked **before**
   idle slots — so a freed worker prefers to start a new download rather than
   park.
3. **Start execution.** For each newly-assigned row that is a real task
   (`idle == FALSE`), call `worker$call(fun, args)` — the download begins on that
   background session; the row goes `running`. Idle rows that received a worker
   just sit at `ready` (a booted, free worker).

The net effect: a conserved set of 4 `r_session` objects **circulates** between
task rows (while downloading) and idle slot rows (while free). The idle rows are
the "home" the workers return to when there is no pending task.

```mermaid
sequenceDiagram
    participant Mgr as queue_manager
    participant Q as task_q (tibble)
    participant W as r_session worker
    participant API as External API + DB

    Mgr->>Q: push(dl_station, station=NL101)
    Q->>Q: schedule(): assign free worker W, state->running
    Q->>W: W$call(dl_station, args)
    W->>API: download measurements + meta, write to DB
    Mgr->>Q: pop()  (every 5s)
    Q->>W: poll connection
    W-->>Q: connection "ready" (finished)
    Q->>Q: schedule(): read result, state->done, return W to idle pool
    Q-->>Mgr: result (or {error: ...})
    Note over Mgr: loops pop() until only the 4 idle rows remain
```

### 4.5 Two AT-specific customizations (not in the vanilla callr example)

These are the parts most likely to confuse someone comparing against the callr
docs:

**(a) Duplicate-station deferral.** The canonical `stop("Duplicate task id")` is
commented out. Instead, if you `push()` a station id that is already in the
table, the new row is marked `state = "duplicate"` and its id becomes
`"<seq> <station>"` (e.g. `.7 NL101`). Because `schedule()` only dispatches
`waiting` rows, a `duplicate` row never runs. On each `schedule()`, the
duplicates block strips the id back to the bare station and, **only if that
station is no longer present**, flips the row to `waiting` so it can run. This
**serializes downloads for the same station**, avoiding two workers hitting the
same station / DB rows concurrently. It was added specifically to fix a
**data-corruption bug** caused by concurrent downloads of the same station.

**(b) Done-task renaming.** When a real task reaches `done`, if its id does not
start with `.` it is renamed to `"<seq> <station>"`. This separates the *logical
station id* from the *row id*, so a not-yet-popped completed task does not look
like a duplicate of a fresh push of the same station, and `pop()` matches rows
unambiguously.

### 4.6 Bookkeeping helpers

`get_num_waiting()`, `get_num_running()`, `get_num_done()`, `is_idle()` (true when
`sum(!idle) == 0`, i.e. only the 4 worker slots remain) and `list_tasks()` expose
queue state. `scripts/test_functions.R::simple_task_list()` and
`modules/view_que.R` render a human-readable view (station / time range / state).

---

## 5. What one worker actually does: `dl_station()`

Each task runs `dl_station(id, time_start, time_end)` (`funs/download_fun.R`)
inside its own background R session. It opens its **own** direct SQLite connection
(not the pool) with a 120 s busy handler, then for one sensor:

1. Downloads the sensor's measurements (`download_data_samenmeten` via
   `ATdatabase::download_data`).
2. Reads the sensor's metadata doc to find its associated **KNMI** weather station
   and **LML** reference stations, downloads those measurements too, and inserts
   any missing station **locations**.
3. Writes everything to `measurements` / `location`, logging to
   `data/download.log`.

The `cache` table records which `(station, start, end)` ranges have been fetched
(used by `station_overview()` to visualise coverage).

---

## 6. Database tables involved

| table          | role                                                                                                                                                       |
|----------------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `meta`         | document store (via `ATdatabase::add_doc/get_doc/list_doc`). Holds `data_req`, `data_req_done`, `schedule`, project/municipality info, `datastream`, `station` docs. **This is the job queue / message bus.** |
| `measurements` | downloaded time-series values (the cache the app reads)                                                                                                     |
| `location`     | station coordinates & metadata                                                                                                                              |
| `cache`        | per-station downloaded date ranges                                                                                                                          |

---

## 7. Configuration & operations

- **Environment variables:** `ANALYSETOGETHER_DATAFOLDER` (DB + lock + logs dir,
  default `./data`), `ANALYSETOGETHER_SCHEDULE` (schedule CSV, default
  `prepped_data/schedule.csv`), `ANALYSETOGETHER_LOGLEVEL` (see
  `funs/logging_fun.R`).
- **Concurrency** is the `task_q$new(concurrency = 4L)` argument;
  `queue_manager.R` uses the default 4 and the drain loop's `> 4` must match it.
  The value 4 is a deliberate balance of limited container size and API
  politeness — experience showed it to be a good mix of download speed and
  keeping both the container and the upstream APIs healthy.
- **Logs:** `data/queue.log` (manager), `data/download.log` (workers).
- **Helper scripts:** `scripts/create_data_request.R` (enqueue test jobs),
  `scripts/queue_test.R` (interactive task_q experimentation),
  `scripts/launch_queue_manager.R` (start manager + generate load),
  `scripts/monitor_db.R` / `scripts/analyse_download_duration.R` (observability).

---

## 8. End-to-end: the app side that produces a job

The producer path lives in `modules/get_data_button.R`:

1. The user selects a municipality/project + period and clicks **Get data**. The
   module reads the cache and shows availability (calendar + map).
2. If data is missing, the user clicks **Download external**
   (`do_download_external`), which:
   - downloads the sensor **metadata** synchronously
     (`download_sensor_meta`),
   - calls `create_data_request()` to write the `data_req` job document(s), and
   - computes the job's **queue position** (`waiting_number`), surfaced to the
     user by the `waiting_info` module.
3. From there the `queue_manager.R` process (§3) picks up the job and fills the
   cache. The user re-checks availability and, once present, visualises the data.

```mermaid
sequenceDiagram
    participant U as User (browser)
    participant App as Shiny (get_data_button)
    participant DB as SQLite (meta/measurements)
    participant QM as queue_manager.R
    participant API as External APIs

    U->>App: Select mun/project + period, "Get data"
    App->>DB: read cached measurements
    App-->>U: show availability (calendar + map)
    U->>App: "Download external" (if gaps)
    App->>DB: create_data_request() -> store data_req job doc
    loop queue manager loop
        QM->>DB: acquire filelock, list data_req jobs
        QM->>QM: task_q R6 pool (4 callr workers)
        QM->>API: dl_station() per station
        API-->>QM: measurements
        QM->>DB: store measurements, mark data_req_done
    end
    U->>App: re-check cache -> data now present -> visualise
```
