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
 * Seeds the known compute platforms using Rust target triple codes. The system
 * tenant owns all records, making them visible to all tenants through RLS.
 * Idempotent: guarded by code lookups so it is safe to run on every database
 * recreation.
 */

\echo '--- Compute Platforms Seed ---'

-- Linux x86-64 (GNU libc)
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
    'x86_64-unknown-linux-gnu',
    'Linux x86-64 (GNU libc)',
    '64-bit x86 Linux using the GNU C Library (glibc). The standard platform '
    'for most Linux server and desktop installations. Compatible with Debian, '
    'Ubuntu, RHEL, and most mainstream distributions.',
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
    where code = 'x86_64-unknown-linux-gnu'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Linux ARM64 (GNU libc)
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
    'aarch64-unknown-linux-gnu',
    'Linux ARM64 (GNU libc)',
    '64-bit ARM (ARMv8-A) Linux using the GNU C Library. Used on AWS Graviton '
    '2/3/4 instances, Ampere Altra servers, and ARM-based developer boards.',
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
    where code = 'aarch64-unknown-linux-gnu'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Linux x86-64 (musl libc)
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
    'x86_64-unknown-linux-musl',
    'Linux x86-64 (musl libc)',
    '64-bit x86 Linux using musl libc. Produces fully statically-linked '
    'binaries with no external libc dependency, suitable for minimal container '
    'images such as Alpine Linux.',
    'linux',
    'x86_64',
    'musl',
    true,
    current_user,
    current_user,
    'system.initial_load',
    '',
    current_timestamp,
    ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_compute_platforms_tbl
    where code = 'x86_64-unknown-linux-musl'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Linux ARM64 (musl libc)
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
    'aarch64-unknown-linux-musl',
    'Linux ARM64 (musl libc)',
    '64-bit ARM Linux using musl libc. Statically-linked binaries for ARM, '
    'suitable for Alpine Linux containers and embedded ARM deployments.',
    'linux',
    'aarch64',
    'musl',
    true,
    current_user,
    current_user,
    'system.initial_load',
    '',
    current_timestamp,
    ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_compute_platforms_tbl
    where code = 'aarch64-unknown-linux-musl'
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
    'x86_64-apple-darwin',
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
    where code = 'x86_64-apple-darwin'
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
    'aarch64-apple-darwin',
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
    where code = 'aarch64-apple-darwin'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Windows x86-64 (MSVC)
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
    'x86_64-pc-windows-msvc',
    'Windows x86-64 (MSVC)',
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
    where code = 'x86_64-pc-windows-msvc'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Windows x86-64 (MinGW)
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
    'x86_64-pc-windows-gnu',
    'Windows x86-64 (MinGW)',
    '64-bit x86 Windows using the GNU toolchain (MinGW-w64). Produces Windows '
    'executables built with GCC without requiring the MSVC runtime. Compatible '
    'with MSYS2 and Cygwin environments.',
    'windows',
    'x86_64',
    'mingw',
    true,
    current_user,
    current_user,
    'system.initial_load',
    '',
    current_timestamp,
    ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_compute_platforms_tbl
    where code = 'x86_64-pc-windows-gnu'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Windows ARM64 (MSVC)
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
    'aarch64-pc-windows-msvc',
    'Windows ARM64 (MSVC)',
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
    where code = 'aarch64-pc-windows-msvc'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

select 'Compute: Platforms' as entity, count(*) as count
from ores_compute_platforms_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
