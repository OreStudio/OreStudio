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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.utility/uuid/tenant_id.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>

namespace ores::utility::uuid {

namespace {

/**
 * @brief Returns the max UUID (all ones) used for the system tenant.
 */
boost::uuids::uuid make_max_uuid() {
    boost::uuids::string_generator gen;
    return gen(max_uuid_str);
}

}

tenant_id::tenant_id(boost::uuids::uuid uuid)
    : uuid_(uuid) {}

tenant_id tenant_id::system() {
    static const auto max_uuid = make_max_uuid();
    return tenant_id(max_uuid);
}

std::expected<tenant_id, std::string>
tenant_id::from_uuid(const boost::uuids::uuid& uuid) {
    if (uuid.is_nil()) {
        return std::unexpected(
            "Cannot create tenant_id from nil UUID. "
            "Use tenant_id::system() for system tenant.");
    }
    return tenant_id(uuid);
}

std::expected<tenant_id, std::string>
tenant_id::from_string(std::string_view str) {
    try {
        boost::uuids::string_generator gen;
        auto uuid = gen(str.begin(), str.end());
        return from_uuid(uuid);
    } catch (const std::exception& e) {
        return std::unexpected(
            std::string("Failed to parse UUID: ") + e.what());
    }
}

bool tenant_id::is_system() const noexcept {
    static const auto max_uuid = make_max_uuid();
    return uuid_ == max_uuid;
}

bool tenant_id::is_nil() const noexcept {
    return uuid_.is_nil();
}

const boost::uuids::uuid& tenant_id::to_uuid() const noexcept {
    return uuid_;
}

std::string tenant_id::to_string() const {
    return boost::uuids::to_string(uuid_);
}

}
