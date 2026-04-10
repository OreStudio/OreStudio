/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * Controller Service Definitions Population Script
 *
 * Seeds the ores_controller_service_definitions_tbl with one row per service
 * that the controller is responsible for launching and monitoring.
 *
 * NATS domain services (args_template = NULL): use the built-in default:
 *   --log-enabled --log-level {log_level} --log-directory {log_dir}
 *   --log-replica-index {replica_index}
 *   --nats-url {nats_url} --nats-subject-prefix {nats_prefix} {nats_tls_args}
 *
 * HTTP/WT/wrapper services have a custom args_template.  Ports use template
 * variables {http_port} and {wt_port} resolved by the controller from the
 * checkout label in the NATS subject prefix.
 *
 * This script is idempotent: each INSERT is guarded by WHERE NOT EXISTS.
 */

do $$
declare
    v_actor         text := 'ddl@system.ores';
    v_reason        text := 'system.initial_load';
    v_commentary    text := 'Initial controller service definition seed';
    v_infinity      timestamptz := ores_utility_infinity_timestamp_fn();
    -- Each row: (service_name, binary_name, desired_replicas, args_template, description)
    -- args_template NULL → supervisor uses built-in default template.
    svc             record;
begin
    for svc in
        select *
        from (values
            -- NATS domain services: standard args template (NULL = use default)
            ('ores.iam.service',        'ores.iam.service',        1, null::text, 'Identity and access management'),
            ('ores.refdata.service',    'ores.refdata.service',    1, null,       'Reference data — currencies, countries, parties'),
            ('ores.dq.service',         'ores.dq.service',         1, null,       'Data quality governance and cataloguing'),
            ('ores.variability.service','ores.variability.service', 1, null,       'Feature flags and system configuration'),
            ('ores.assets.service',     'ores.assets.service',     1, null,       'Digital assets — images and tags'),
            ('ores.scheduler.service',  'ores.scheduler.service',  1, null,       'Job scheduling and execution'),
            ('ores.reporting.service',  'ores.reporting.service',  1, null,       'Risk report generation and management'),
            -- Telemetry: custom args to set local1 NATS monitoring port (8221).
            ('ores.telemetry.service', 'ores.telemetry.service', 1,
                '--log-enabled --log-level {log_level} --log-directory {log_dir}'
                ' --log-replica-index {replica_index}'
                ' --nats-url {nats_url} --nats-subject-prefix {nats_prefix}'
                ' {nats_tls_args}'
                ' --nats-monitor-url http://localhost:8221',
                'Service telemetry and health monitoring'),
            ('ores.trading.service',    'ores.trading.service',    1, null,       'Trading instruments and trades'),
            ('ores.compute.service',    'ores.compute.service',    1, null,       'Compute grid orchestration'),
            ('ores.synthetic.service',  'ores.synthetic.service',  1, null,       'Synthetic data generation'),
            ('ores.workflow.service',   'ores.workflow.service',   1, null,       'Workflow execution engine'),
            -- ore-service: custom args to set work-dir under publish/var (not /var/ores).
            ('ores.ore.service', 'ores.ore.service', 1,
                '--log-enabled --log-level {log_level} --log-directory {log_dir}'
                ' --log-replica-index {replica_index}'
                ' --nats-url {nats_url} --nats-subject-prefix {nats_prefix}'
                ' {nats_tls_args}'
                ' --work-dir ../var/ore-service/work',
                'ORE pricing engine integration'),
            ('ores.marketdata.service', 'ores.marketdata.service', 1, null,       'Market data and price series'),
            ('ores.analytics.service', 'ores.analytics.service',  1, null,       'Analytics pricing engine integration'),
            -- HTTP server: needs --port and --storage-dir.
            -- {http_port} is resolved by the controller from the checkout label.
            ('ores.http.server', 'ores.http.server', 1,
                '--log-enabled --log-level {log_level} --log-directory {log_dir}'
                ' --log-replica-index {replica_index}'
                ' --nats-url {nats_url} --nats-subject-prefix {nats_prefix}'
                ' {nats_tls_args}'
                ' --port {http_port} --storage-dir ../storage',
                'HTTP API gateway'),
            -- WT server: Wt args passed after --.
            -- {wt_port} is resolved by the controller from the checkout label.
            ('ores.wt.service', 'ores.wt.service', 1,
                '--log-enabled --log-level {log_level} --log-directory {log_dir}'
                ' --log-replica-index {replica_index}'
                ' --nats-url {nats_url} --nats-subject-prefix {nats_prefix}'
                ' {nats_tls_args}'
                ' -- --http-address 0.0.0.0 --docroot . --http-port {wt_port}',
                'Web UI server (Wt framework)'),
            -- Compute wrapper nodes: 5 replicas, one per grid node.
            -- {host_id} = stable UUID derived from hostname:replica_index.
            -- {work_dir} = ../run/wrappers/node_N.
            -- All replicas share the same certificate (ores.compute.wrapper.crt).
            ('ores.compute.wrapper', 'ores.compute.wrapper', 5,
                '--log-enabled --log-level {log_level} --log-directory {log_dir}'
                ' --log-replica-index {replica_index}'
                ' --nats-url {nats_url} --nats-subject-prefix {nats_prefix}'
                ' {nats_tls_args}'
                ' --host-id {host_id} --tenant-id {tenant_id}'
                ' --work-dir {work_dir} --http-base-url http://localhost:{http_port}',
                'Compute grid worker nodes')
        ) as t(service_name, binary_name, desired_replicas, args_template, description)
    loop
        if not exists (
            select 1 from ores_controller_service_definitions_tbl
            where service_name = svc.service_name
              and valid_to = v_infinity
        ) then
            insert into ores_controller_service_definitions_tbl (
                id,
                version,
                service_name,
                binary_name,
                desired_replicas,
                restart_policy,
                max_restart_count,
                enabled,
                args_template,
                description,
                modified_by,
                performed_by,
                change_reason_code,
                change_commentary,
                valid_from,
                valid_to
            ) values (
                gen_random_uuid(),
                0,
                svc.service_name,
                svc.binary_name,
                svc.desired_replicas,
                'always',
                3,
                1,
                svc.args_template,
                svc.description,
                v_actor,
                v_actor,
                v_reason,
                v_commentary,
                current_timestamp,
                v_infinity
            );
        end if;
    end loop;
end;
$$;

-- Summary
select 'Controller Service Definitions' as entity, count(*) as count
from ores_controller_service_definitions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
