// SPDX-License-Identifier: GPL-3.0-or-later
import { spawn } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { z } from 'zod';

const databaseName = z.string().regex(/^[a-zA-Z_][a-zA-Z0-9_]{0,62}$/);
export const cloneConfig = z
    .object({
        source: databaseName,
        method: z.enum(['dump', 'template']).default('dump'),
        host: z.enum(['127.0.0.1', 'localhost', '::1']).default('127.0.0.1'),
        port: z.number().int().min(1).max(65535).default(5432),
        user: z.string().min(1),
        bin: z.string().min(1).optional(),
        passfile: z.string().min(1).optional(),
        timeoutMs: z.number().int().min(1000).max(3600000).default(600000),
        seedReports: z.boolean().default(true),
        indexLegacyTicks: z.boolean().default(false),
    })
    .strict()
    .refine(
        (c) => !['postgres', 'template0', 'template1'].includes(c.source.toLowerCase()),
        'Choose an OreStudio source database.',
    );

const markerSql = `
with source_book as (
    select b.* from ores_refdata_books_tbl b
    where tenant_id = :'tenant'::uuid and party_id = :'party'::uuid
      and valid_to = ores_utility_infinity_timestamp_fn()
    order by id limit 1
), inserted as (
    insert into ores_refdata_books_tbl
    select (jsonb_populate_record(null::ores_refdata_books_tbl,
        to_jsonb(b) || jsonb_build_object('id', :'markerId', 'name', :'markerName',
            'version', 0, 'description', 'Disposable agent integration database marker',
            'gl_account_ref', null, 'cost_center', null, 'sandbox_id', null,
            'is_sweepable', false))).*
    from source_book b returning id
)
select id from inserted;`;

// Scope the change to active report-trigger jobs, leaving the scheduler's
// infrastructure jobs available. Insert versions through OreStudio's trigger.
const reportJobs = `
    valid_to = ores_utility_infinity_timestamp_fn()
    and is_active
    and (job_name like 'report_definition.%'
         or action_payload ->> 'subject' = 'reporting.v1.ops.trigger_report_instance')
`;
const pauseReportJobs = `
with paused as (
    insert into ores_scheduler_job_definitions_tbl
    select (jsonb_populate_record(null::ores_scheduler_job_definitions_tbl,
        to_jsonb(j) || jsonb_build_object('is_active', false,
            'change_reason_code', 'system.update',
            'change_commentary', 'Pause copied report schedule for agent integration'))).*
    from ores_scheduler_job_definitions_tbl j
    where ${reportJobs}
    returning id
)
select json_build_object('paused', count(*), 'since', clock_timestamp()) from paused;
`;
const reportIsolation = `
select json_build_object(
    'active', (select count(*) from ores_scheduler_job_definitions_tbl where ${reportJobs}),
    'triggered', (select count(*) from ores_reporting_report_trigger_events_tbl
        where trigger_type = 'scheduled' and triggered_at >= :'since'::timestamptz)
);
`;

async function pgCommand(file, args, env, timeout, input) {
    const child = spawn(file, args, {
        env,
        windowsHide: true,
        stdio: ['pipe', 'pipe', 'pipe'],
        signal: AbortSignal.timeout(timeout),
    });
    let out = '',
        err = '';
    child.stdout.setEncoding('utf8').on('data', (chunk) => {
        out = (out + chunk).slice(-20000);
    });
    child.stderr.setEncoding('utf8').on('data', (chunk) => {
        err = (err + chunk).slice(-12000);
    });
    child.stdin.on('error', () => {}); // The client can reject a connection before reading SQL.
    child.stdin.end(input);
    const code = await new Promise((resolve, reject) => {
        child.once('error', reject);
        child.once('close', resolve);
    });
    if (code !== 0) throw Error('PostgreSQL command failed (' + code + '): ' + err.trim());
    return out.trim();
}

export function databaseClone(config, base) {
    const suffix = randomUUID().replaceAll('-', '');
    const name = 'ores_agent_it_' + suffix;
    const marker = { id: randomUUID(), name: 'Agent DB marker ' + suffix };
    const env = {
        ...process.env,
        PGHOST: config.host,
        PGPORT: String(config.port),
        PGUSER: config.user,
        PGCONNECT_TIMEOUT: '8',
    };
    for (const key of ['PGDATABASE', 'PGSERVICE', 'PGSERVICEFILE', 'PGOPTIONS']) delete env[key];
    if (config.passfile) env.PGPASSFILE = resolve(base, config.passfile);
    const program = (tool) =>
        config.bin
            ? resolve(base, config.bin, tool + (process.platform === 'win32' ? '.exe' : ''))
            : tool;
    const run = (tool, args, input) => pgCommand(program(tool), args, env, config.timeoutMs, input);
    const psql = (db, sql, vars = {}) =>
        run(
            'psql',
            [
                '-X',
                '-w',
                '-qAt',
                '-d',
                db,
                '-v',
                'ON_ERROR_STOP=1',
                ...Object.entries(vars).flatMap(([key, value]) => ['-v', key + '=' + value]),
            ],
            sql,
        );
    let created = false;
    return {
        name,
        marker,
        serviceEnv: {
            ORES_DATABASE_NAME: name,
            ORES_AGENT_CLONE_DATABASE: name,
            ORES_DB_HOST: config.host,
            ORES_DB_PORT: String(config.port),
            PGDATABASE: name,
            PGHOST: config.host,
            PGPORT: String(config.port),
        },
        async restore() {
            if (config.method === 'template') {
                await run('createdb', [
                    '-w',
                    '--maintenance-db=postgres',
                    '--template=' + config.source,
                    '--strategy=file_copy',
                    name,
                ]);
                created = true;
                return;
            }
            const scratch = await mkdtemp(join(tmpdir(), 'ores-agent-pgdump-'));
            try {
                const dump = join(scratch, 'snapshot.dump');
                await run('pg_dump', ['-w', '-Fc', '-d', config.source, '-f', dump]);
                await run('createdb', [
                    '-w',
                    '--maintenance-db=postgres',
                    '--template=template0',
                    name,
                ]);
                created = true;
                await run('pg_restore', [
                    '-w',
                    '--exit-on-error',
                    '--single-transaction',
                    '-d',
                    name,
                    dump,
                ]);
            } finally {
                await rm(scratch, { recursive: true, force: true });
            }
        },
        async installMarker(tenant, party) {
            const result = await this.sql(markerSql, {
                tenant,
                party,
                markerId: marker.id,
                markerName: marker.name,
            });
            if (result !== marker.id)
                throw Error('The clone has no active book for the selected tenant and party.');
        },
        async sql(sql, vars) {
            if (!created || (await psql(name, 'select current_database();')) !== name)
                throw Error('SQL seeding requires the database created by this run.');
            return psql(name, sql, vars);
        },
        async pauseReportSchedules() {
            return JSON.parse(await this.sql(pauseReportJobs));
        },
        async verifyReportIsolation(since) {
            const state = JSON.parse(await this.sql(reportIsolation, { since }));
            if (state.active || state.triggered)
                throw Error(
                    'Report schedule isolation failed: ' +
                        state.active +
                        ' active jobs and ' +
                        state.triggered +
                        ' new scheduled triggers.',
                );
            return state;
        },
        async indexLegacyTicks() {
            const state = JSON.parse(
                await this.sql(`
                select json_build_object(
                    'legacy', exists (
                        select 1 from pg_attribute
                        where attrelid = 'ores_marketdata_ticks_tbl'::regclass
                          and attname = 'series_id' and not attisdropped
                    ),
                    'indexed', exists (
                        select 1 from pg_index
                        where indrelid = 'ores_marketdata_ticks_tbl'::regclass
                          and indisvalid and indpred is null
                          and pg_get_indexdef(indexrelid, 1, true) = 'series_id'
                          and pg_get_indexdef(indexrelid, 2, true) = 'coord'
                          and pg_get_indexdef(indexrelid, 3, true) = 'ts'
                    )
                );
            `),
            );
            if (state.legacy && !state.indexed) {
                await this.sql(`
                    create index ores_agent_legacy_tick_coordinate_idx
                    on ores_marketdata_ticks_tbl (series_id, coord, ts desc);
                    analyze ores_marketdata_ticks_tbl;
                `);
            }
            return { ...state, created: state.legacy && !state.indexed };
        },
        async drop() {
            if (!created) return;
            // The caller stops the owned fleet first. Terminate only sessions in
            // the generated database, including queries left by stopped workers.
            await run('dropdb', ['-w', '--maintenance-db=postgres', '--force', name]);
            created = false;
        },
    };
}
