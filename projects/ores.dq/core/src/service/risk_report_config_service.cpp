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
#include "ores.dq.core/service/risk_report_config_service.hpp"
#include "ores.dq.api/domain/risk_report_config.hpp"
#include "ores.dq.api/messaging/risk_report_config_protocol.hpp"
#include "ores.dq.core/repository/risk_report_config_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>
// Log lines stream uuids with uuid_io's operator<<, which the include check
// does not count as a use.
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp> // IWYU pragma: keep.

using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

namespace ores::dq::service {

using namespace ores::logging;

risk_report_config_service::risk_report_config_service(context ctx)
    : ctx_(std::move(ctx)) {}
namespace {

/**
 * @brief The current row a key names, or an empty vector when there is none.
 *
 * A key record carries each column with the column's own type, and the
 * repository takes the text form every one of its key parameters shares, so
 * the conversion lives here rather than at every call site.
 *
 * The key record carries the key the model declares, which is the one a caller
 * holds. When that is not the storage key the row is found by it and the
 * repository's storage-key read is not used at all.
 */
std::vector<domain::risk_report_config> read_one(repository::risk_report_config_repository& repo,
                                                 const ores::database::context& ctx,
                                                 const messaging::risk_report_config_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

}

messaging::list_risk_report_configs_response risk_report_config_service::list_risk_report_configs(
    const messaging::list_risk_report_configs_request& request) {
    messaging::list_risk_report_configs_response response;
    if (!request.order.field.empty() &&
        !repository::risk_report_config_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "risk report configs", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    response.configs =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_config_count(ctx_, request.filter);
    return response;
}

messaging::get_risk_report_config_response risk_report_config_service::get_risk_report_config(
    const messaging::get_risk_report_config_request& request) {
    messaging::get_risk_report_config_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "risk_report_config"});
        return response;
    }
    response.risk_report_config = std::move(found.front());
    return response;
}

messaging::get_many_risk_report_configs_response
risk_report_config_service::get_many_risk_report_configs(
    const messaging::get_many_risk_report_configs_request& request) {
    messaging::get_many_risk_report_configs_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::risk_report_config_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.risk_report_config = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::risk_report_config>
risk_report_config_service::list_configs(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all risk report configs";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t risk_report_config_service::count_configs() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total risk report configs count";
    return repo_.get_total_config_count(ctx_);
}


std::optional<domain::risk_report_config>
risk_report_config_service::get_config(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting risk report config. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::risk_report_config>
risk_report_config_service::get_configs(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void risk_report_config_service::save_config(const domain::risk_report_config& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Risk Report Config id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving risk report config. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved risk report config. " << "id: " << v.id;
}

void risk_report_config_service::save_configs(
    const std::vector<domain::risk_report_config>& configs) {
    for (const auto& e : configs) {
        if (e.id.is_nil())
            throw std::invalid_argument("Risk Report Config id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << configs.size() << " risk report configs";
    auto ts = configs;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void risk_report_config_service::delete_config(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing risk report config. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed risk report config. " << "id: " << id;
}

void risk_report_config_service::delete_configs(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}


}
