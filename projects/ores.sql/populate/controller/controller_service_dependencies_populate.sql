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
 * Service Dependencies Population Script
 *
 * Seeds ores_controller_service_dependencies_tbl. All NATS domain services
 * depend on ores.iam.service because they fetch the JWKS public key from IAM
 * on startup to validate JWTs.
 *
 * This script is idempotent (ON CONFLICT DO NOTHING).
 */

insert into ores_controller_service_dependencies_tbl (service_name, depends_on)
values
    ('ores.refdata.service',    'ores.iam.service'),
    ('ores.dq.service',         'ores.iam.service'),
    ('ores.variability.service','ores.iam.service'),
    ('ores.assets.service',     'ores.iam.service'),
    ('ores.scheduler.service',  'ores.iam.service'),
    ('ores.reporting.service',  'ores.iam.service'),
    ('ores.telemetry.service',  'ores.iam.service'),
    ('ores.trading.service',    'ores.iam.service'),
    ('ores.compute.service',    'ores.iam.service'),
    ('ores.synthetic.service',  'ores.iam.service'),
    ('ores.workflow.service',   'ores.iam.service'),
    ('ores.ore.service',        'ores.iam.service'),
    ('ores.marketdata.service', 'ores.iam.service')
on conflict do nothing;

-- Summary
select 'Service Dependencies' as entity, count(*) as count
from ores_controller_service_dependencies_tbl;
