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
#include "ores.iam.core/service/login_info_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::iam::service {

using namespace ores::logging;

login_info_service::login_info_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::login_info> login_info_service::list_login_info(std::uint32_t offset,
                                                                    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all login info";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t login_info_service::count_login_info() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total login info count";
    return repo_.get_total_login_info_count(ctx_);
}


std::optional<domain::login_info>
login_info_service::get_login_info(const std::string& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting login info. " << "account_id: " << account_id;
    auto results = repo_.read_latest(ctx_, account_id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void login_info_service::save_login_info(const domain::login_info& v) {
    if (v.account_id.is_nil())
        throw std::invalid_argument("Login Info account_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving login info. " << "account_id: " << v.account_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved login info. " << "account_id: " << v.account_id;
}

void login_info_service::save_login_info(const std::vector<domain::login_info>& login_info) {
    for (const auto& e : login_info) {
        if (e.account_id.is_nil())
            throw std::invalid_argument("Login Info account_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << login_info.size() << " login info";
    auto ts = login_info;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void login_info_service::delete_login_info(const std::string& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing login info. " << "account_id: " << account_id;
    repo_.remove(ctx_, account_id);
    BOOST_LOG_SEV(lg(), info) << "Removed login info. " << "account_id: " << account_id;
}

void login_info_service::delete_login_info(const std::vector<std::string>& account_ids) {
    repo_.remove(ctx_, account_ids);
}


}
