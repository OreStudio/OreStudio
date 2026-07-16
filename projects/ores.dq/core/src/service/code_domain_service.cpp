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
#include "ores.dq.core/service/code_domain_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

code_domain_service::code_domain_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::code_domain> code_domain_service::list_domains(std::uint32_t offset,
                                                                   std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all code domains";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t code_domain_service::count_domains() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total code domains count";
    return repo_.get_total_domain_count(ctx_);
}

std::optional<domain::code_domain>
code_domain_service::get_domain_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting code domain at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::code_domain> code_domain_service::get_domain(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting code domain: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void code_domain_service::save_domain(const domain::code_domain& v) {
    if (v.code.empty())
        throw std::invalid_argument("Code Domain code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving code domain: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved code domain: " << v.code;
}

void code_domain_service::save_domains(const std::vector<domain::code_domain>& domains) {
    for (const auto& e : domains)
        if (e.code.empty())
            throw std::invalid_argument("Code Domain code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << domains.size() << " code domains";
    auto ts = domains;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void code_domain_service::delete_domain(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing code domain: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed code domain: " << code;
}

void code_domain_service::delete_domains(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::code_domain> code_domain_service::get_domain_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for code domain: " << code;
    return repo_.read_all(ctx_, code);
}

}
