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
#include "ores.refdata.core/service/calendar_rule_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

calendar_rule_service::calendar_rule_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::calendar_rule> calendar_rule_service::list_calendar_rules(std::uint32_t offset,
                                                                              std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all calendar rules";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t calendar_rule_service::count_calendar_rules() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total calendar rules count";
    return repo_.get_total_calendar_rule_count(ctx_);
}


std::vector<domain::calendar_rule> calendar_rule_service::list_calendar_rules_by_calendar_code(
    const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing calendar rules by calendar_code: " << calendar_code;
    return repo_.read_latest_by_calendar_code(ctx_, calendar_code, offset, limit);
}

std::uint32_t
calendar_rule_service::count_calendar_rules_by_calendar_code(const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total calendar rules count by calendar_code: "
                               << calendar_code;
    return repo_.get_total_calendar_rule_count_by_calendar_code(ctx_, calendar_code);
}

std::vector<domain::calendar_rule>
calendar_rule_service::list_calendar_rules_by_calendar_code_as_of(
    const std::string& calendar_code,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug) << "Listing calendar rules by calendar_code as of window: "
                               << calendar_code;
    return repo_.read_by_calendar_code_as_of(ctx_, calendar_code, valid_from_bound, valid_to_bound);
}
std::optional<domain::calendar_rule>
calendar_rule_service::get_calendar_rule_at_version(const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting calendar rule at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::calendar_rule>
calendar_rule_service::get_calendar_rule(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting calendar rule. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void calendar_rule_service::save_calendar_rule(const domain::calendar_rule& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Calendar Rule id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving calendar rule. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved calendar rule. " << "id: " << v.id;
}

void calendar_rule_service::save_calendar_rules(
    const std::vector<domain::calendar_rule>& calendar_rules) {
    for (const auto& e : calendar_rules) {
        if (e.id.is_nil())
            throw std::invalid_argument("Calendar Rule id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << calendar_rules.size() << " calendar rules";
    auto ts = calendar_rules;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void calendar_rule_service::delete_calendar_rule(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing calendar rule. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed calendar rule. " << "id: " << id;
}

void calendar_rule_service::delete_calendar_rules(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::calendar_rule>
calendar_rule_service::get_calendar_rule_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for calendar rule. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
