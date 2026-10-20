\set ON_ERROR_STOP on
\timing off
set client_min_messages = notice;

drop schema if exists tickprobe cascade;
create schema tickprobe;

-- Shape a0: the observation table as it is: same columns, same indexes, same insert trigger.
create table tickprobe.obs_a0 (like public.ores_marketdata_market_observations_tbl including all);
select create_hypertable('tickprobe.obs_a0', 'observation_datetime', chunk_time_interval => interval '30 days');

create function tickprobe.obs_a0_ins() returns trigger as $$
begin
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);
    update tickprobe.obs_a0
    set valid_to = current_timestamp
    where tenant_id = new.tenant_id
      and series_id = new.series_id
      and observation_datetime = new.observation_datetime
      and oresmd_uri = new.oresmd_uri
      and valid_to = ores_utility_infinity_timestamp_fn()
      and valid_from < current_timestamp;
    new.valid_from := current_timestamp;
    new.valid_to := ores_utility_infinity_timestamp_fn();
    return new;
end;
$$ language plpgsql;
create trigger obs_a0_ins_trg before insert on tickprobe.obs_a0
for each row execute function tickprobe.obs_a0_ins();

-- Shape a1: the same table and indexes, with no trigger.
create table tickprobe.obs_a1 (like public.ores_marketdata_market_observations_tbl including all);
select create_hypertable('tickprobe.obs_a1', 'observation_datetime', chunk_time_interval => interval '30 days');

-- Shape b: an append-only tick table with one index.
create table tickprobe.tick_append (
    series_id uuid not null,
    ts timestamptz not null,
    coord smallint not null default 0,
    value double precision not null
);
create index tick_append_series_ts_idx on tickprobe.tick_append (series_id, ts desc);
select create_hypertable('tickprobe.tick_append', 'ts', chunk_time_interval => interval '1 day');

create procedure tickprobe.run(target text, batch int, batches int) language plpgsql as $$
declare
    pool uuid[];
    t0 timestamptz;
    secs numeric;
    b int;
    base timestamptz := now() - interval '2 hours';
    tenant uuid := '540caae1-5ccc-43c9-82a8-5a49c5136d58';
    party uuid := 'e93025eb-c4bf-4df0-ba80-11574a8f3d67';
    total int := batch * batches;
begin
    select array_agg(gen_random_uuid()) into pool from generate_series(1, 2000);
    t0 := clock_timestamp();
    for b in 1 .. batches loop
        if target = 'a0' then
            insert into tickprobe.obs_a0 (id, tenant_id, party_id, series_id, observation_datetime, oresmd_uri, value, source, valid_from, valid_to)
            select gen_random_uuid(), tenant, party, pool[1 + (g % 2000)],
                   base + ((b * batch + g) * interval '10 microseconds'),
                   'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TRY',
                   '43.98966624067043', 'synthetic.bloomberg.usdtry', now(), 'infinity'
            from generate_series(1, batch) g;
        elsif target = 'a1' then
            insert into tickprobe.obs_a1 (id, tenant_id, party_id, series_id, observation_datetime, oresmd_uri, value, source, valid_from, valid_to)
            select gen_random_uuid(), tenant, party, pool[1 + (g % 2000)],
                   base + ((b * batch + g) * interval '10 microseconds'),
                   'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TRY',
                   '43.98966624067043', 'synthetic.bloomberg.usdtry', now(), 'infinity'
            from generate_series(1, batch) g;
        else
            insert into tickprobe.tick_append (series_id, ts, coord, value)
            select pool[1 + (g % 2000)], base + ((b * batch + g) * interval '10 microseconds'), 0, 43.98966624067043
            from generate_series(1, batch) g;
        end if;
        commit;
    end loop;
    secs := extract(epoch from clock_timestamp() - t0);
    raise notice 'RESULT shape=% batch=% rows=% secs=% rows_per_s=%', target, batch, total, round(secs, 2), round(total / secs);
end
$$;

call tickprobe.run('a0', 1, 1500);
call tickprobe.run('a0', 100, 200);
call tickprobe.run('a0', 1000, 100);
call tickprobe.run('a0', 10000, 20);

call tickprobe.run('a1', 1, 1500);
call tickprobe.run('a1', 100, 200);
call tickprobe.run('a1', 1000, 100);
call tickprobe.run('a1', 10000, 20);

call tickprobe.run('b', 1, 1500);
call tickprobe.run('b', 100, 200);
call tickprobe.run('b', 1000, 100);
call tickprobe.run('b', 10000, 20);

-- Size for each row, with the indexes.
select 'a0' as shape, count(*) as rows, hypertable_size('tickprobe.obs_a0') as bytes,
       round(hypertable_size('tickprobe.obs_a0')::numeric / count(*)) as bytes_per_row from tickprobe.obs_a0
union all
select 'a1', count(*), hypertable_size('tickprobe.obs_a1'),
       round(hypertable_size('tickprobe.obs_a1')::numeric / count(*)) from tickprobe.obs_a1
union all
select 'b', count(*), hypertable_size('tickprobe.tick_append'),
       round(hypertable_size('tickprobe.tick_append')::numeric / count(*)) from tickprobe.tick_append;

-- COPY from a client file into shapes a1 and b, 300,000 rows each.
\copy (select gen_random_uuid(), '540caae1-5ccc-43c9-82a8-5a49c5136d58'::uuid, 'e93025eb-c4bf-4df0-ba80-11574a8f3d67'::uuid, gen_random_uuid(), now() + g * interval '10 microseconds', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TRY', '43.98966624067043', 'synthetic.bloomberg.usdtry', now(), 'infinity'::timestamptz from generate_series(1, 300000) g) to '/tmp/claude-1000/-home-marco-Development-OreStudio-ores-dev-brave-hopper/cd92ddb4-109c-4d46-a8bd-130519a3d10d/scratchpad/copy_a.csv' csv
\copy (select gen_random_uuid(), now() + g * interval '10 microseconds', 0, 43.98966624067043 from generate_series(1, 300000) g) to '/tmp/claude-1000/-home-marco-Development-OreStudio-ores-dev-brave-hopper/cd92ddb4-109c-4d46-a8bd-130519a3d10d/scratchpad/copy_b.csv' csv
\timing on
\copy tickprobe.obs_a1 (id, tenant_id, party_id, series_id, observation_datetime, oresmd_uri, value, source, valid_from, valid_to) from '/tmp/claude-1000/-home-marco-Development-OreStudio-ores-dev-brave-hopper/cd92ddb4-109c-4d46-a8bd-130519a3d10d/scratchpad/copy_a.csv' csv
\copy tickprobe.tick_append (series_id, ts, coord, value) from '/tmp/claude-1000/-home-marco-Development-OreStudio-ores-dev-brave-hopper/cd92ddb4-109c-4d46-a8bd-130519a3d10d/scratchpad/copy_b.csv' csv
\timing off

drop schema tickprobe cascade;
