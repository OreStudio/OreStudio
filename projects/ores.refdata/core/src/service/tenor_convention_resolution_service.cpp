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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.core/service/tenor_convention_resolution_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <stdexcept>
#include <utility>

namespace ores::refdata::service {

using namespace ores::logging;
using ores::service::messaging::stamp;

tenor_convention_resolution_service::tenor_convention_resolution_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_(ctx) {}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_service::list_resolutions() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenor convention resolutions";
    return repo_.read_latest();
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_service::list_resolutions(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenor convention resolutions with offset: " << offset
                               << " limit: " << limit;
    return repo_.read_latest(offset, limit);
}

std::uint32_t tenor_convention_resolution_service::get_total_resolution_count() {
    return repo_.get_total_resolution_count();
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_service::list_resolutions_by_convention(
    const std::string& convention_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing tenor convention resolutions for convention: "
                               << convention_code;
    return repo_.read_latest_by_convention(convention_code);
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_service::list_resolutions_by_convention(
    const std::string& convention_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing tenor convention resolutions for convention: "
                               << convention_code << " offset: " << offset << " limit: " << limit;
    return repo_.read_latest_by_convention(convention_code, offset, limit);
}

std::uint32_t tenor_convention_resolution_service::get_total_resolution_count_by_convention(
    const std::string& convention_code) {
    return repo_.get_total_resolution_count_by_convention(convention_code);
}

std::uint32_t tenor_convention_resolution_service::get_total_resolution_count_by_tenor(
    const std::string& tenor_code) {
    return repo_.get_total_resolution_count_by_tenor(tenor_code);
}

}
