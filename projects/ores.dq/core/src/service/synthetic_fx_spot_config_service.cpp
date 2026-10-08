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
#include "ores.dq.core/service/synthetic_fx_spot_config_service.hpp"
#include "ores.dq.api/domain/synthetic_fx_spot_config.hpp"
#include "ores.dq.api/messaging/synthetic_fx_spot_config_protocol.hpp"
#include "ores.dq.core/repository/synthetic_fx_spot_config_repository.hpp"
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

synthetic_fx_spot_config_service::synthetic_fx_spot_config_service(context ctx)
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
std::vector<domain::synthetic_fx_spot_config>
read_one(repository::synthetic_fx_spot_config_repository& repo,
         const ores::database::context& ctx,
         const messaging::synthetic_fx_spot_config_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

}

messaging::list_synthetic_fx_spot_configs_response
synthetic_fx_spot_config_service::list_synthetic_fx_spot_configs(
    const messaging::list_synthetic_fx_spot_configs_request& request) {
    messaging::list_synthetic_fx_spot_configs_response response;
    if (!request.order.field.empty() &&
        !repository::synthetic_fx_spot_config_repository::is_sortable(request.order.field)) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "synthetic FX spot configs", .field = request.order.field});
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

messaging::get_synthetic_fx_spot_config_response
synthetic_fx_spot_config_service::get_synthetic_fx_spot_config(
    const messaging::get_synthetic_fx_spot_config_request& request) {
    messaging::get_synthetic_fx_spot_config_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "synthetic_fx_spot_config"});
        return response;
    }
    response.synthetic_fx_spot_config = std::move(found.front());
    return response;
}

messaging::get_many_synthetic_fx_spot_configs_response
synthetic_fx_spot_config_service::get_many_synthetic_fx_spot_configs(
    const messaging::get_many_synthetic_fx_spot_configs_request& request) {
    messaging::get_many_synthetic_fx_spot_configs_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::synthetic_fx_spot_config_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.synthetic_fx_spot_config = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::synthetic_fx_spot_config>
synthetic_fx_spot_config_service::list_configs(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all synthetic FX spot configs";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t synthetic_fx_spot_config_service::count_configs() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total synthetic FX spot configs count";
    return repo_.get_total_config_count(ctx_);
}


std::optional<domain::synthetic_fx_spot_config>
synthetic_fx_spot_config_service::get_config(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting synthetic FX spot config. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::synthetic_fx_spot_config>
synthetic_fx_spot_config_service::get_configs(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void synthetic_fx_spot_config_service::save_config(const domain::synthetic_fx_spot_config& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Synthetic FX Spot Config id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving synthetic FX spot config. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved synthetic FX spot config. " << "id: " << v.id;
}

void synthetic_fx_spot_config_service::save_configs(
    const std::vector<domain::synthetic_fx_spot_config>& configs) {
    for (const auto& e : configs) {
        if (e.id.is_nil())
            throw std::invalid_argument("Synthetic FX Spot Config id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << configs.size() << " synthetic FX spot configs";
    auto ts = configs;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void synthetic_fx_spot_config_service::delete_config(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing synthetic FX spot config. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed synthetic FX spot config. " << "id: " << id;
}

void synthetic_fx_spot_config_service::delete_configs(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}


}
