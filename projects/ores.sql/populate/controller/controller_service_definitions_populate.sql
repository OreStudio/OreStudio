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
 * Seeds the ores_controller_service_definitions_tbl with one row per NATS
 * domain service that the controller is responsible for launching and
 * monitoring. The controller itself, the HTTP server, and the WT server are
 * NOT included — they are started by the shell launch script or have
 * non-standard arg patterns.
 *
 * args_template is NULL so the supervisor uses the built-in default template:
 *   --log-enabled --log-level {log_level} --log-directory {log_dir}
 *   --log-replica-index {replica_index}
 *   --nats-url {nats_url} --nats-subject-prefix {nats_prefix}
 *
 * This script is idempotent: each INSERT is guarded by WHERE NOT EXISTS.
 */

do $$
declare
    v_actor         text := 'ddl@system.ores';
    v_reason        text := 'system.initial_load';
    v_commentary    text := 'Initial controller service definition seed';
    v_infinity      timestamptz := ores_utility_infinity_timestamp_fn();
    svc_name        text;
    svc_binary      text;
begin
    for svc_name, svc_binary in
        values
            ('ores.iam.service',        'ores.iam.service'),
            ('ores.refdata.service',    'ores.refdata.service'),
            ('ores.dq.service',         'ores.dq.service'),
            ('ores.variability.service','ores.variability.service'),
            ('ores.assets.service',     'ores.assets.service'),
            ('ores.scheduler.service',  'ores.scheduler.service'),
            ('ores.reporting.service',  'ores.reporting.service'),
            ('ores.telemetry.service',  'ores.telemetry.service'),
            ('ores.trading.service',    'ores.trading.service'),
            ('ores.compute.service',    'ores.compute.service'),
            ('ores.synthetic.service',  'ores.synthetic.service'),
            ('ores.workflow.service',   'ores.workflow.service'),
            ('ores.ore.service',        'ores.ore.service'),
            ('ores.marketdata.service', 'ores.marketdata.service')
    loop
        if not exists (
            select 1 from ores_controller_service_definitions_tbl
            where service_name = svc_name
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
                modified_by,
                performed_by,
                change_reason_code,
                change_commentary,
                valid_from,
                valid_to
            ) values (
                gen_random_uuid(),
                0,
                svc_name,
                svc_binary,
                1,
                'always',
                3,
                1,
                null,
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
