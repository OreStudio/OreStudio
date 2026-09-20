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
#include "ores.refdata.core/service/currency_currency_group_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <stdexcept>
#include <utility>

namespace ores::refdata::service {

using namespace ores::logging;
using ores::service::messaging::stamp;


currency_currency_group_service::currency_currency_group_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_(ctx_) {}

std::vector<domain::currency_currency_group>
currency_currency_group_service::list_currency_groups() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currency groups";
    return repo_.read_latest();
}

std::vector<domain::currency_currency_group>
currency_currency_group_service::list_currency_groups(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currency groups with offset: " << offset
                               << " limit: " << limit;
    return repo_.read_latest(offset, limit);
}

std::uint32_t currency_currency_group_service::get_total_currency_group_count() {
    return repo_.get_total_currency_group_count();
}

std::vector<domain::currency_currency_group>
currency_currency_group_service::list_currency_groups_by_currency(
    const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing currency groups for currency: " << currency_iso_code;
    return repo_.read_latest_by_currency(currency_iso_code);
}

std::vector<domain::currency_currency_group>
currency_currency_group_service::list_currency_groups_by_currency(
    const std::string& currency_iso_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing currency groups for currency: " << currency_iso_code
                               << " offset: " << offset << " limit: " << limit;
    return repo_.read_latest_by_currency(currency_iso_code, offset, limit);
}

std::uint32_t currency_currency_group_service::get_total_currency_group_count_by_currency(
    const std::string& currency_iso_code) {
    return repo_.get_total_currency_group_count_by_currency(currency_iso_code);
}

std::uint32_t currency_currency_group_service::get_total_currency_group_count_by_group(
    const std::string& currency_group_code) {
    return repo_.get_total_currency_group_count_by_group(currency_group_code);
}

void currency_currency_group_service::save_currency_group(
    const domain::currency_currency_group& currency_group) {
    if (currency_group.currency_iso_code.empty()) {
        throw std::invalid_argument("Currency cannot be empty.");
    }
    if (currency_group.currency_group_code.empty()) {
        throw std::invalid_argument("Group cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving currency group: " << currency_group.currency_iso_code
                               << "/" << currency_group.currency_group_code;
    auto t = currency_group;
    stamp(t, ctx_);
    repo_.write(t);
    BOOST_LOG_SEV(lg(), info) << "Saved currency group: " << currency_group.currency_iso_code << "/"
                              << currency_group.currency_group_code;
}

void currency_currency_group_service::save_currency_groups(
    const std::vector<domain::currency_currency_group>& currency_groups) {
    for (const auto& e : currency_groups) {
        if (e.currency_iso_code.empty()) {
            throw std::invalid_argument("Currency cannot be empty.");
        }
        if (e.currency_group_code.empty()) {
            throw std::invalid_argument("Group cannot be empty.");
        }
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << currency_groups.size() << " currency groups";
    auto ts = currency_groups;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ts);
    BOOST_LOG_SEV(lg(), info) << "Saved " << currency_groups.size() << " currency groups";
}

void currency_currency_group_service::remove_currency_group(
    const std::string& currency_iso_code, const std::string& currency_group_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency group: " << currency_iso_code << "/"
                               << currency_group_code;
    repo_.remove(currency_iso_code, currency_group_code);
    BOOST_LOG_SEV(lg(), info) << "Removed currency group: " << currency_iso_code << "/"
                              << currency_group_code;
}

}
