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
#include "ores.dq.core/service/data_domain_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

data_domain_service::data_domain_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::data_domain> data_domain_service::list_domains(std::uint32_t offset,
                                                                   std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all data domains";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t data_domain_service::count_domains() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total data domains count";
    return repo_.get_total_domain_count(ctx_);
}

std::optional<domain::data_domain>
data_domain_service::get_domain_at_version(const std::string& name, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting data domain at version: " << name
                               << " version: " << version;
    return repo_.read_at_version(ctx_, name, version);
}

std::optional<domain::data_domain> data_domain_service::get_domain(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting data domain: " << name;
    auto results = repo_.read_latest(ctx_, name);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void data_domain_service::save_domain(const domain::data_domain& v) {
    if (v.name.empty())
        throw std::invalid_argument("Data Domain name cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving data domain: " << v.name;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved data domain: " << v.name;
}

void data_domain_service::save_domains(const std::vector<domain::data_domain>& domains) {
    for (const auto& e : domains)
        if (e.name.empty())
            throw std::invalid_argument("Data Domain name cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << domains.size() << " data domains";
    auto ts = domains;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void data_domain_service::delete_domain(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing data domain: " << name;
    repo_.remove(ctx_, name);
    BOOST_LOG_SEV(lg(), info) << "Removed data domain: " << name;
}

void data_domain_service::delete_domains(const std::vector<std::string>& names) {
    repo_.remove(ctx_, names);
}

std::vector<domain::data_domain> data_domain_service::get_domain_history(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for data domain: " << name;
    return repo_.read_all(ctx_, name);
}

}
