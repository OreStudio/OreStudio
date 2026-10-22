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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Generator: projects/ores.sql/utility/generate_setup_ore_parameters_populate.py
 *
 * Setup Ore Parameters Population Script
 *
 * Seeds one definition for each Setup parameter of the engine settings, with
 * the text a screen shows for it. This script is idempotent.
 */

\echo '--- Setup Ore Parameters ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'implyTodaysFixings'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'implyTodaysFixings', 1, 'boolean', false,
            'Imply today’s fixings', 'Whether today’s fixings are implied.',
            'Decides what the run does about the fixing of the valuation date. When it is No, the fixing for the valuation date is loaded from the fixing file like any earlier fixing. When it is Yes, that fixing is not loaded and the engine implies it from the curves. Use Yes when the run prices off hypothetical market data, as in scenario analysis and stress testing, because a fixing from the real market would not match the hypothetical curves.',
            'No. Today’s fixing is loaded.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: implyTodaysFixings'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'includeTodaysCashFlows'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'includeTodaysCashFlows', 2, 'boolean', false,
            'Include today’s cash flows', 'Whether cash flows that pay today count.',
            'Decides whether a cash flow that pays on the valuation date is part of the NPV or is treated as matured and excluded. It is one of two QuantLib flags that ORE exposes. A choice here applies to every analytic of the run. The simulation analytic has its own parameter of the same name, which applies to the XVA, XVA sensitivity and XVA stress analytics. Change it only when the NPV must agree with a system that treats payments on the valuation date the other way.',
            'No. A cash flow that pays on the valuation date is treated as matured.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: includeTodaysCashFlows'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'includeReferenceDateEvents'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'includeReferenceDateEvents', 3, 'boolean', false,
            'Include reference date events', 'Whether events on the reference date count.',
            'Decides whether an event that falls on the valuation date is still counted in the NPV or is treated as matured and excluded. It is the companion of Include today’s cash flows, and the two together decide how a trade is valued on the day of its payment or event. A choice here applies to every analytic of the run, and the simulation analytic has its own parameter of the same name.',
            'No. An event on the valuation date is treated as matured.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: includeReferenceDateEvents'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'enrichIndexFixings'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'enrichIndexFixings', 4, 'boolean', false,
            'Enrich index fixings', 'Whether gaps in index fixings are filled.',
            'Fills gaps in the history of an index. When it is Yes, a missing fixing is replaced by a fallback: the previous fixing first, and the next fixing when there is no previous one. Ignore fixing lag and Ignore fixing lead limit how far the fallback may be from the gap. Use it when the fixing file has gaps. It changes the numbers, because a trade that reads a filled fixing is valued with it.',
            'No. A gap stays a gap.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: enrichIndexFixings'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'ignoreFixingLag'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'ignoreFixingLag', 5, 'integer', false,
            'Ignore fixing lag', 'A number of days. Zero turns the check off.',
            'Works with Enrich index fixings. When the engine fills a gap in an index’s fixings with the previous fixing, this setting limits how old that previous fixing may be. If the previous fixing is more days older than the gap than this number, the engine does not use it as the fallback. A value of zero turns the check off, so a fixing of any age can fill the gap.',
            '0. The check is off.', 'A whole number of days, zero or more.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: ignoreFixingLag'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'ignoreFixingLead'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'ignoreFixingLead', 6, 'integer', false,
            'Ignore fixing lead', 'A number of days. Zero turns the check off.',
            'Works with Enrich index fixings. When the engine fills a gap in an index’s fixings with the next fixing, this setting limits how far ahead that next fixing may be. If the next fixing date leads the gap by more days than this number, the engine does not use it as the fallback. A value of zero turns the check off, so a fixing of any distance can fill the gap.',
            '0. The check is off.', 'A whole number of days, zero or more.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: ignoreFixingLead'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'observationModel'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'observationModel', 7, 'string', false,
            'Observation model', 'How the engine handles QuantLib notifications.',
            'Controls how many QuantLib observer notifications the engine uses when market data and fixings are updated and when the evaluation date moves. Notifications keep every dependent object up to date, and they cost time in a simulation. None is the normal QuantLib behaviour, where every update notifies every dependent object. The other three choices cut that cost in different ways and give a similar speed up. Disable switches all notifications off during market and fixing updates and when the evaluation date moves along a path, and the engine triggers the needed recalculations itself. Defer switches notifications off during market and fixing updates and runs the updates when notifications are on again. Unregister keeps notifications on but unregisters floating rate coupons from their indices, which removes most of them. The ORE user guide describes the setting as a control of performance during simulation.',
            'None.', 'One of None, Disable, Defer, Unregister.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: observationModel'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'lazyMarketBuilding'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'lazyMarketBuilding', 8, 'boolean', false,
            'Lazy market building', 'Whether the market is built on demand.',
            'Decides when the engine builds the curves listed in today’s market. When it is Yes, a curve is built only when a trade or analytic asks for it, which saves time when the configuration lists curves that the portfolio never uses. When it is No, every curve in today’s market is built at the start.',
            'Yes. Curves are built on demand.', 'A flag, written Y or N, or true or false.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: lazyMarketBuilding'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'buildFailedTrades'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'buildFailedTrades', 9, 'boolean', false,
            'Build failed trades', 'Whether a failed trade is replaced by a dummy.',
            'Decides what happens to a trade that cannot be loaded or built. When it is Yes, the engine builds a dummy trade of type Failed, with zero notional and zero NPV, so the trade still appears in the portfolio. When it is No, the failed trade is left out. Yes keeps the trade count honest, but a reader must know that a Failed trade has no value.',
            'Yes. A failed trade becomes a dummy trade.', 'A flag, written Y or N, or true or false.',
            '1.8.7.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: buildFailedTrades'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'continueOnError'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'continueOnError', 10, 'boolean', false,
            'Continue on error', 'Whether the run goes on after an error.',
            'Decides whether the engine stops at the first error or goes on. When it is Yes, the engine does not exit on an error but tries to continue the processing, so one bad curve or trade does not lose the whole run, and the reports may be incomplete. When it is No, the run exits at the error.',
            'Yes. The run goes on after an error.', 'A flag, written Y or N, or true or false.',
            '1.8.4.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: continueOnError'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'useAtParCouponsCurves'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'useAtParCouponsCurves', 11, 'boolean', false,
            'At par coupons for curves', 'Par approximation or indexed ibor coupons for curves.',
            'Decides how the engine models a floating rate coupon when it builds the curves. When it is Yes, it uses the par approximation of the coupon. When it is No, it uses indexed ibor coupons. The choice goes back to the QuantLib flag QL_USE_INDEXED_COUPON and its run time setting. Yes is also the default of a QuantLib build.',
            'Yes. The par approximation.', 'A flag, written Y or N, or true or false.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: useAtParCouponsCurves'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'useAtParCouponsTrades'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'useAtParCouponsTrades', 12, 'boolean', false,
            'At par coupons for trades', 'Par approximation or indexed ibor coupons for trades.',
            'Decides how the engine models a floating rate coupon when it builds the trades. When it is Yes, it uses the par approximation of the coupon. When it is No, it uses indexed ibor coupons. It is the companion of At par coupons for curves, and the two can differ. The choice goes back to the QuantLib flag QL_USE_INDEXED_COUPON and its run time setting.',
            'Yes. The par approximation.', 'A flag, written Y or N, or true or false.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: useAtParCouponsTrades'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'nThreads'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'nThreads', 13, 'integer', false,
            'Threads', 'A whole number of at least 1.',
            'The number of threads the engine may use. More than one thread speeds up the analytics that spread their work over scenarios or paths: the sensitivity analytic, the classic exposure simulation and the AMC exposure simulation. These are the three analytics that the ORE user guide names.',
            '1.', 'A whole number, at least 1.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: nThreads'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'logMask'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'logMask', 14, 'integer', false,
            'Log mask', 'A bit set, such as 31.',
            'Decides which messages the engine writes to its log file. Each kind of message has a bit: 1 alert, 2 critical, 4 error, 8 warning, 16 notice, 32 debug and 64 data. Add the bits of the kinds you want. Common totals are 15 for alert to warning, 31 for the same and notice, 63 for the same and debug, and 127 for the same and data. A higher total writes more and makes a larger log. The value 255 gives the most detail.',
            '15. Alert, critical, error and warning.', 'A whole number. A bit set of the message kinds.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: logMask'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'gzipCompressionLevel'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', 'gzipCompressionLevel', 15, 'integer', false,
            'Gzip compression level', 'A whole number from 0 to 9.',
            'Sets how strongly the engine compresses the zipped cube files it writes. The value is a whole number from 0 to 9. A value of 0 means no compression, which gives the fastest write and the largest file. A value of 9 means the strongest compression, which gives the slowest write and the smallest file. A value outside 0 to 9 is an error. The default of 6 balances speed and size.',
            '6. A balance of speed and size.', 'A whole number from 0 to 9.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: gzipCompressionLevel'
        );
    end if;

end $$;
