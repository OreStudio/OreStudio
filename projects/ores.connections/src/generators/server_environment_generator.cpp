/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.connections/generators/server_environment_generator.hpp"

#include <faker-cxx/word.h>
#include <faker-cxx/internet.h>
#include <faker-cxx/lorem.h>
#include <faker-cxx/number.h>
#include <faker-cxx/string.h>
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::connections::generators {

using ores::utility::uuid::uuid_v7_generator;

domain::server_environment generate_synthetic_server_environment() {
    static uuid_v7_generator gen;

    domain::server_environment r;
    r.id = gen();
    r.folder_id = std::nullopt;
    r.name = std::string(faker::word::noun()) + " Server";
    r.host = std::string(faker::internet::domainName());
    r.port = faker::number::integer(1024, 65535);
    r.username = std::string(faker::internet::username());
    r.encrypted_password = "";  // Not encrypted by generator
    r.description = std::string(faker::lorem::sentence());

    return r;
}

domain::server_environment generate_synthetic_server_environment(
    const boost::uuids::uuid& folder_id) {
    auto r = generate_synthetic_server_environment();
    r.folder_id = folder_id;
    return r;
}

std::vector<domain::server_environment> generate_synthetic_server_environments(std::size_t n) {
    std::vector<domain::server_environment> r;
    r.reserve(n);
    for (std::size_t i = 0; i < n; ++i) {
        r.push_back(generate_synthetic_server_environment());
    }
    return r;
}

}
