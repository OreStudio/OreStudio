// SPDX-License-Identifier: GPL-3.0-or-later
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import test from 'node:test';
import { cloneConfig, databaseClone } from '../evals/native-clone.mjs';

test('native clone restricts the source to a named local application database', () => {
    assert.equal(cloneConfig.safeParse({ source: 'postgres', user: 'local' }).success, false);
    assert.equal(
        cloneConfig.safeParse({ source: 'development', host: 'example.com', user: 'local' })
            .success,
        false,
    );
    assert.equal(
        cloneConfig.safeParse({ source: 'dbname=production', user: 'local' }).success,
        false,
    );
});

const bin = process.env.ORES_AGENT_TEST_PG_BIN;
const port = Number(process.env.ORES_AGENT_TEST_PG_PORT);
test(
    'real PostgreSQL clones the snapshot, seeds missing rows, and drops only the clone',
    { skip: !bin || !port },
    async () => {
        const source = 'agent_test_source_' + randomUUID().replaceAll('-', '');
        const user = process.env.ORES_AGENT_TEST_PG_USER ?? 'agent_clone_admin';
        const env = { ...process.env, PGHOST: '127.0.0.1', PGPORT: String(port), PGUSER: user };
        const pg = (tool, args) =>
            execFileSync(join(bin, tool + (process.platform === 'win32' ? '.exe' : '')), args, {
                env,
                encoding: 'utf8',
                windowsHide: true,
            }).trim();
        const psql = (db, statement) =>
            pg('psql', ['-X', '-w', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', db, '-c', statement]);
        const clone = databaseClone(cloneConfig.parse({ source, user, bin, port }), '.');
        const ids = Object.fromEntries(
            [
                'tenant',
                'party',
                'run',
                'partialRun',
                'riskRun',
                'portfolioId',
                'book',
                'trade',
                'otherTrade',
                'definition',
            ].map((key) => [key, randomUUID()]),
        );
        const {
            tenant,
            party,
            run,
            partialRun,
            riskRun,
            portfolioId,
            book,
            trade,
            otherTrade,
            definition,
        } = ids;
        pg('createdb', ['-w', '--maintenance-db=postgres', source]);
        try {
            psql(
                source,
                [
                    "create function ores_utility_infinity_timestamp_fn() returns timestamptz language sql immutable as $$ select 'infinity'::timestamptz $$",
                    'create table ores_refdata_portfolios_tbl (id uuid, tenant_id uuid, party_id uuid, name text, parent_portfolio_id uuid, valid_from timestamptz, valid_to timestamptz)',
                    'create table ores_refdata_books_tbl (id uuid primary key, tenant_id uuid, party_id uuid, version integer, name text, description text, parent_portfolio_id uuid, valid_from timestamptz, valid_to timestamptz)',
                    'create table ores_trading_trade_bookings_tbl (tenant_id uuid, trade_id uuid, book_id uuid, valid_from timestamptz, valid_to timestamptz)',
                    'create table ores_reporting_report_instances_tbl (id uuid primary key, tenant_id uuid, party_id uuid, definition_id uuid, name text, description text, version integer, started_at timestamptz, completed_at timestamptz, priced_at timestamptz, output_message text, output_storage_key text, valid_to timestamptz)',
                    'create table ores_reporting_report_metrics_tbl (id uuid primary key, tenant_id uuid, party_id uuid, report_instance_id uuid, result_id uuid, as_of timestamptz, measure text, source text, line integer, report_definition_id uuid, family text, subject_type text, subject_id text, currency text, scenario text, factor text, qualifiers text, value double precision, valid_from timestamptz, valid_to timestamptz)',
                    'create table ores_reporting_report_error_events_tbl (id uuid primary key, tenant_id uuid, party_id uuid, report_instance_id uuid, scope_code text, severity text, message text, event_time timestamptz)',
                    "create table ores_scheduler_job_definitions_tbl (id uuid, job_name text, action_payload jsonb, is_active boolean, change_reason_code text, change_commentary text, valid_to timestamptz default 'infinity')",
                    'create table ores_reporting_report_trigger_events_tbl (trigger_type text, triggered_at timestamptz)',
                    'create table ores_marketdata_ticks_tbl (series_id uuid, coord smallint, ts timestamptz)',
                    'create index test_legacy_series_time_idx on ores_marketdata_ticks_tbl (series_id, ts desc)',
                    "create table ores_reporting_report_definitions_tbl (id uuid, tenant_id uuid, party_id uuid, name text, report_type text, pre_processing text, post_processing text, scheduler_job_id uuid, schedule_expression text, cut_id uuid, cut_kind_code text, prepared_input_key text, valid_to timestamptz default 'infinity')",
                    "create table ores_reporting_report_configurations_tbl (tenant_id uuid, party_id uuid, report_definition_id uuid, configuration_id uuid, configuration_type_code text, valid_to timestamptz default 'infinity')",
                    "create table ores_reporting_risk_report_configs_tbl (id uuid, tenant_id uuid, party_id uuid, configuration_id uuid, market_data_type text, market_data_date date, valid_to timestamptz default 'infinity')",
                    "create table ores_reporting_risk_report_config_books_tbl (tenant_id uuid, risk_report_config_id uuid, valid_to timestamptz default 'infinity')",
                    'create table ores_reporting_risk_report_config_portfolios_tbl (tenant_id uuid, risk_report_config_id uuid, portfolio_id uuid, valid_from timestamptz, valid_to timestamptz)',
                    "create table ores_reporting_configuration_analytics_tbl (tenant_id uuid, configuration_id uuid, analytic_type_code text, active text, valid_to timestamptz default 'infinity')",
                    "create function test_report_definition_version() returns trigger language plpgsql as $$ begin update ores_reporting_report_definitions_tbl set valid_to=clock_timestamp() where id=new.id and valid_to='infinity'; return new; end $$",
                    'create trigger test_report_definition_version before insert on ores_reporting_report_definitions_tbl for each row execute function test_report_definition_version()',
                    `insert into ores_reporting_report_definitions_tbl (id, tenant_id, party_id, name, report_type, pre_processing, post_processing, schedule_expression, prepared_input_key)
                       values ('${definition}', '${tenant}', '${party}', 'Risk test', 'risk', 'substitute', 'execute', '', 'prepared/test')`,
                    `insert into ores_reporting_report_configurations_tbl (tenant_id,party_id,report_definition_id,configuration_id,configuration_type_code)
                       values ('${tenant}', '${party}', '${definition}', '${definition}', 'risk_report_config')`,
                    `insert into ores_reporting_risk_report_configs_tbl (id,tenant_id,party_id,configuration_id,market_data_type,market_data_date)
                       values ('${definition}', '${tenant}', '${party}', '${definition}', 'date', '2026-01-01')`,
                    `insert into ores_reporting_configuration_analytics_tbl (tenant_id,configuration_id,analytic_type_code,active)
                       values ('${tenant}', '${definition}', 'sensitivity', 'Y')`,
                    "create function test_scheduler_version() returns trigger language plpgsql as $$ begin update ores_scheduler_job_definitions_tbl set valid_to=clock_timestamp() where id=new.id and valid_to='infinity'; return new; end $$",
                    'create trigger test_scheduler_version before insert on ores_scheduler_job_definitions_tbl for each row execute function test_scheduler_version()',
                    `insert into ores_scheduler_job_definitions_tbl (id, job_name, action_payload, is_active)
                       values (gen_random_uuid(), 'report_definition.test', '{}', true),
                              (gen_random_uuid(), 'custom name', '{"subject":"reporting.v1.ops.trigger_report_instance"}', true),
                              (gen_random_uuid(), 'inbox sweep', '{}', true)`,
                    "insert into ores_refdata_portfolios_tbl values ('" +
                        portfolioId +
                        "','" +
                        tenant +
                        "','" +
                        party +
                        "','Portfolio',null,'2020-01-01','infinity')",
                    "insert into ores_refdata_books_tbl values ('" +
                        book +
                        "','" +
                        tenant +
                        "','" +
                        party +
                        "',1,'Book',null,'" +
                        portfolioId +
                        "','2020-01-01','infinity')",
                    "insert into ores_trading_trade_bookings_tbl select '" +
                        tenant +
                        "', x::uuid,'" +
                        book +
                        "','2020-01-01','infinity' from unnest(array['" +
                        trade +
                        "','" +
                        otherTrade +
                        "']) x",
                    "insert into ores_reporting_report_instances_tbl select x::uuid,'" +
                        tenant +
                        "','" +
                        party +
                        "','" +
                        definition +
                        "',x,null,1,'2026-01-01','2026-01-02','2026-01-01',null,null,'infinity' from unnest(array['" +
                        run +
                        "','" +
                        riskRun +
                        "']) x",
                    "insert into ores_reporting_report_metrics_tbl values (gen_random_uuid(),'" +
                        tenant +
                        "','" +
                        party +
                        "','" +
                        run +
                        "',gen_random_uuid(),'2026-01-01','NPV','npv.csv',2,'" +
                        definition +
                        "','npv_report','trade','" +
                        trade +
                        "','GBP','','','{}',42,'2026-01-01','infinity')",
                ].join(';'),
            );
            await assert.rejects(
                clone.sql('select current_database()'),
                /requires the database created/,
            );
            await clone.restore();
            await clone.installMarker(tenant, party);
            assert.deepEqual(await clone.indexLegacyTicks(), {
                legacy: true,
                indexed: false,
                created: true,
            });
            assert.deepEqual(await clone.indexLegacyTicks(), {
                legacy: true,
                indexed: true,
                created: false,
            });
            assert.equal(
                psql(
                    source,
                    "select count(*) from pg_indexes where indexname='ores_agent_legacy_tick_coordinate_idx'",
                ),
                '0',
            );
            const isolation = await clone.pauseReportSchedules();
            assert.equal(isolation.paused, 2);
            assert.deepEqual(await clone.verifyReportIsolation(isolation.since), {
                active: 0,
                triggered: 0,
            });
            assert.equal((await clone.pauseReportSchedules()).paused, 0);
            assert.equal(
                await clone.sql(
                    "select count(*) from ores_scheduler_job_definitions_tbl where is_active and valid_to='infinity'",
                ),
                '1',
            );
            assert.equal(
                psql(
                    source,
                    "select count(*) from ores_scheduler_job_definitions_tbl where is_active and valid_to='infinity'",
                ),
                '3',
            );
            await clone.sql(
                "insert into ores_reporting_report_trigger_events_tbl values ('manual', clock_timestamp())",
            );
            assert.deepEqual(await clone.verifyReportIsolation(isolation.since), {
                active: 0,
                triggered: 0,
            });
            await clone.sql(
                "insert into ores_reporting_report_trigger_events_tbl values ('scheduled', clock_timestamp())",
            );
            await assert.rejects(
                clone.verifyReportIsolation(isolation.since),
                /1 new scheduled triggers/,
            );

            const sql = await readFile(
                new URL('../evals/native-report-seed.sql', import.meta.url),
                'utf8',
            );
            const vars = {
                tenant,
                party,
                run,
                partialRun,
                riskRun,
                portfolio: 'Portfolio',
                scenario: 'Agent integration',
                riskDefinition: 'Risk test',
                riskDate: '2026-01-01',
            };
            assert.deepEqual(JSON.parse(await clone.sql(sql, vars)), {
                riskDefinitionPrepared: true,
                riskPortfolioAdded: true,
                partialCopied: true,
                riskTrades: 2,
                vegaSeeded: true,
                scenarioSeeded: true,
            });
            assert.deepEqual(JSON.parse(await clone.sql(sql, vars)), {
                riskDefinitionPrepared: false,
                riskPortfolioAdded: false,
                partialCopied: false,
                riskTrades: 2,
                vegaSeeded: false,
                scenarioSeeded: false,
            });
            assert.equal(
                await clone.sql('select count(*) from ores_reporting_report_metrics_tbl'),
                '18',
            );
            assert.equal(
                await clone.sql('select count(*) from ores_reporting_report_error_events_tbl'),
                '1',
            );
            assert.equal(
                psql(source, 'select count(*) from ores_reporting_report_metrics_tbl'),
                '1',
            );
            assert.equal(psql(source, 'select count(*) from ores_refdata_books_tbl'), '1');
            await clone.drop();
            assert.equal(
                psql(
                    'postgres',
                    "select count(*) from pg_database where datname = '" + clone.name + "'",
                ),
                '0',
            );
        } finally {
            try {
                await clone.drop();
            } finally {
                pg('dropdb', ['-w', '--maintenance-db=postgres', source]);
            }
        }
    },
);
