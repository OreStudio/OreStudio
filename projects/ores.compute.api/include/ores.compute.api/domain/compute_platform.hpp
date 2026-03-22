/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#ifndef ORES_COMPUTE_DOMAIN_COMPUTE_PLATFORM_HPP
#define ORES_COMPUTE_DOMAIN_COMPUTE_PLATFORM_HPP

#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::compute::domain {

/**
 * @brief A known compute platform, identified by its Rust target triple code.
 *
 * System-owned data: owned by the system tenant and visible read-only to all
 * tenants via RLS.
 */
struct compute_platform final {
    boost::uuids::uuid id;
    std::string code;          // e.g. "x86_64-unknown-linux-gnu"
    std::string display_name;  // e.g. "Linux x86-64 (GNU libc)"
    std::string description;
    std::string os_family;     // linux / macos / windows
    std::string cpu_arch;      // x86_64 / aarch64
    std::optional<std::string> abi; // gnu / musl / msvc / mingw / nullopt
    bool is_active = true;
};

}

#endif
