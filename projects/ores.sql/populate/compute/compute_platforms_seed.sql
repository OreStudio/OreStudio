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
 * Compute Platforms Seed Script
 *
 * Seeds the known compute platforms using vcpkg target triplet codes so that
 * the same identifier is used across the build system (VCPKG_TARGET_TRIPLET),
 * runtime (ORES_PLATFORM_TRIPLET stamped into each binary) and the database.
 * The system tenant owns all records, making them visible to all tenants
 * through RLS. Idempotent: guarded by code lookups so it is safe to run on
 * every database recreation.
 */

\echo '--- Compute Platforms Seed ---'

-- Linux x86-64
insert into ores_compute_platforms_tbl (
    id, tenant_id, version, code, display_name, description,
    os_family, cpu_arch, abi, is_active,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    1,
    'x64-linux',
    'Linux x86-64',
    '64-bit x86 Linux. The standard platform for most Linux server and '
    'desktop installations. Compatible with Debian, Ubuntu, RHEL, and most '
    'mainstream distributions.',
    'linux',
    'x86_64',
    'gnu',
    true,
    current_user,
    current_user,
    'system.initial_load',
    '',
    current_timestamp,
    ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_compute_platforms_tbl
    where code = 'x64-linux'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Linux ARM64
insert into ores_compute_platforms_tbl (
    id, tenant_id, version, code, display_name, description,
    os_family, cpu_arch, abi, is_active,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    1,
    'arm64-linux',
    'Linux ARM64',
    '64-bit ARM (ARMv8-A) Linux. Used on AWS Graviton 2/3/4 instances, '
    'Ampere Altra servers, and ARM-based developer boards.',
    'linux',
    'aarch64',
    'gnu',
    true,
    current_user,
    current_user,
    'system.initial_load',
    '',
    current_timestamp,
    ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_compute_platforms_tbl
    where code = 'arm64-linux'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- macOS Intel
insert into ores_compute_platforms_tbl (
    id, tenant_id, version, code, display_name, description,
    os_family, cpu_arch, abi, is_active,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    1,
    'x64-osx',
    'macOS Intel',
    '64-bit x86 macOS for Intel processors. Covers all Intel Mac hardware '
    'produced between 2006 and 2023. Requires macOS 10.12 Sierra or later '
    'for most modern toolchains.',
    'macos',
    'x86_64',
    null,
    true,
    current_user,
    current_user,
    'system.initial_load',
    '',
    current_timestamp,
    ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_compute_platforms_tbl
    where code = 'x64-osx'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- macOS Apple Silicon (M-series)
insert into ores_compute_platforms_tbl (
    id, tenant_id, version, code, display_name, description,
    os_family, cpu_arch, abi, is_active,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    1,
    'arm64-osx',
    'macOS Apple Silicon (M-series)',
    '64-bit ARM macOS for Apple Silicon (M1, M2, M3 and later). All new Mac '
    'hardware since November 2020. Native ARM execution with no Rosetta 2 '
    'translation layer.',
    'macos',
    'aarch64',
    null,
    true,
    current_user,
    current_user,
    'system.initial_load',
    '',
    current_timestamp,
    ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_compute_platforms_tbl
    where code = 'arm64-osx'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Windows x86-64
insert into ores_compute_platforms_tbl (
    id, tenant_id, version, code, display_name, description,
    os_family, cpu_arch, abi, is_active,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    1,
    'x64-windows',
    'Windows x86-64',
    '64-bit x86 Windows using the Microsoft Visual C++ runtime (MSVC ABI). '
    'Standard for native Windows applications and the recommended target for '
    'Windows deployment. Requires the MSVC redistributable at runtime.',
    'windows',
    'x86_64',
    'msvc',
    true,
    current_user,
    current_user,
    'system.initial_load',
    '',
    current_timestamp,
    ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_compute_platforms_tbl
    where code = 'x64-windows'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Windows ARM64
insert into ores_compute_platforms_tbl (
    id, tenant_id, version, code, display_name, description,
    os_family, cpu_arch, abi, is_active,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    1,
    'arm64-windows',
    'Windows ARM64',
    '64-bit ARM Windows using the MSVC runtime. Targets ARM-based Windows '
    'devices including Microsoft Surface Pro X, Surface Pro 11, and Qualcomm '
    'Snapdragon X Elite laptops.',
    'windows',
    'aarch64',
    'msvc',
    true,
    current_user,
    current_user,
    'system.initial_load',
    '',
    current_timestamp,
    ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_compute_platforms_tbl
    where code = 'arm64-windows'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

select 'Compute: Platforms' as entity, count(*) as count
from ores_compute_platforms_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
