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
#include "ores.analytics.core/service/pricing_model_service.hpp"
#include "ores.analytics.api/domain/pricing_model.hpp"
#include "ores.analytics.api/messaging/pricing_model_protocol.hpp"
#include "ores.analytics.core/repository/pricing_model_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using ores::service::messaging::of_acting_party;
using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

namespace ores::analytics::service {

using namespace ores::logging;

pricing_model_service::pricing_model_service(context ctx)
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
std::vector<domain::pricing_model>
pricing_model_read_one(repository::pricing_model_repository& repo,
                       const ores::database::context& ctx,
                       const messaging::pricing_model_key& key) {
    return repo.read_latest(ctx, key.code);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::pricing_model_key pricing_model_key_from(const domain::pricing_model& v) {
    messaging::pricing_model_key key;
    key.code = v.code;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::pricing_model pricing_model_to_domain(const messaging::pricing_model_write& write) {
    domain::pricing_model v;
    v.code = write.code;
    v.description = write.description;
    v.detail = write.detail;
    v.source = write.source;
    return v;
}


}

messaging::list_pricing_models_response
pricing_model_service::list_pricing_models(const messaging::list_pricing_models_request& request) {
    messaging::list_pricing_models_response response;
    if (!request.order.field.empty() &&
        !repository::pricing_model_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "pricing models", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->code_one_of &&
        request.filter->code_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "code_one_of", .limit = "1000"});
        return response;
    }
    // A stated instant is checked here, so a malformed one is the caller's
    // mistake rather than a database error. The caller's text is what the
    // store reads, so a fraction of a second is kept.
    std::optional<std::string> as_of;
    if (request.as_of) {
        as_of = ores::database::repository::parse_as_of(*request.as_of);
        if (!as_of) {
            response.result = refuse(outcome_code::as_of_invalid, {.value = *request.as_of});
            return response;
        }
    }
    response.models = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_model_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_pricing_model_response
pricing_model_service::get_pricing_model(const messaging::get_pricing_model_request& request) {
    messaging::get_pricing_model_response response;
    auto found = pricing_model_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "pricing_model"});
        return response;
    }
    response.pricing_model = std::move(found.front());
    return response;
}

messaging::get_many_pricing_models_response pricing_model_service::get_many_pricing_models(
    const messaging::get_many_pricing_models_request& request) {
    messaging::get_many_pricing_models_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::pricing_model_lookup entry;
        entry.key = k;
        auto found = pricing_model_read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.pricing_model = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_pricing_model_response
pricing_model_service::put_pricing_model(const messaging::put_pricing_model_request& request) {
    messaging::put_pricing_model_response response;
    domain::pricing_model value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written =
        of_acting_party(pricing_model_read_one(repo_, ctx_, pricing_model_key_from(value)), ctx_);
    if (!written.empty())
        response.pricing_model = std::move(written.front());
    return response;
}

messaging::put_many_pricing_models_response pricing_model_service::put_many_pricing_models(
    const messaging::put_many_pricing_models_request& request) {
    messaging::put_many_pricing_models_response response;
    std::vector<domain::pricing_model> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::pricing_model value;
        const auto result = prepare_change(change, request.intent, value);
        if (result.outcome != ores::utility::domain::outcome::ok) {
            // Nothing has been written: the whole set is checked before any
            // of it lands, so a refused element refuses the batch.
            response.result = result;
            return response;
        }
        batch.push_back(std::move(value));
    }
    // One statement, so the set lands together. The store checks each row's
    // claim inside that statement, which is what makes the check above and the
    // write one decision rather than two.
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(request.changes.size());
    for (const auto& change : request.changes)
        claims.push_back(change.precondition);
    repo_.write(ctx_, batch, claims);
    response.models.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = of_acting_party(
            pricing_model_read_one(repo_, ctx_, pricing_model_key_from(value)), ctx_);
        response.models.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_pricing_model_response pricing_model_service::delete_pricing_model(
    const messaging::delete_pricing_model_request& request) {
    messaging::delete_pricing_model_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    if (request.removal.precondition.kind == precondition_kind::must_not_exist) {
        response.result = refuse(outcome_code::precondition_not_supported);
        return response;
    }
    std::optional<std::uint32_t> expected;
    if (request.removal.precondition.kind == precondition_kind::must_match_version) {
        if (!request.removal.precondition.version) {
            response.result = refuse(outcome_code::precondition_incomplete);
            return response;
        }
        expected = request.removal.precondition.version;
    }
    switch (repo_.remove(ctx_, request.removal.key.code, expected)) {
        case repository::pricing_model_repository::remove_status::removed:
            break;
        case repository::pricing_model_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "pricing_model"});
            break;
        case repository::pricing_model_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = pricing_model_read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "pricing_model",
                 .field = "code",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::pricing_model_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_pricing_models_response pricing_model_service::delete_many_pricing_models(
    const messaging::delete_many_pricing_models_request& request) {
    messaging::delete_many_pricing_models_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    for (const auto& removal : request.removals) {
        if (removal.precondition.kind != precondition_kind::any) {
            // The store removes a set in one statement, which carries no
            // per-row version. Refusing is the only answer that keeps the
            // batch atomic: serving it as a sequence of single removals would
            // leave a partial batch behind as soon as one row had moved on.
            response.result = refuse(outcome_code::batch_removal_is_unconditional);
            return response;
        }
    }
    if (request.removals.empty())
        return response;
    std::vector<std::string> code_keys;
    code_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        code_keys.push_back(removal.key.code);
    repo_.remove(ctx_, code_keys);
    return response;
}

messaging::list_pricing_model_versions_response pricing_model_service::list_pricing_model_versions(
    const messaging::list_pricing_model_versions_request& request) {
    messaging::list_pricing_model_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "pricing models", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result = refuse(outcome_code::filter_not_supported, {.entity = "pricing models"});
        return response;
    }
    auto all = repo_.read_all(ctx_, request.key.code);
    // The store reads versions newest first, and the order a caller gets when
    // it states none is key order, which for a version key is oldest first.
    std::reverse(all.begin(), all.end());
    response.total = all.size();
    const auto begin = std::min<std::size_t>(request.offset, all.size());
    const auto end = std::min<std::size_t>(begin + request.limit, all.size());
    response.versions.assign(std::make_move_iterator(all.begin() + begin),
                             std::make_move_iterator(all.begin() + end));
    return response;
}

messaging::get_pricing_model_version_response pricing_model_service::get_pricing_model_version(
    const messaging::get_pricing_model_version_request& request) {
    messaging::get_pricing_model_version_response response;
    auto found = repo_.read_at_version(ctx_, request.key.pricing_model.code, request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "pricing_model"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
pricing_model_service::prepare_change(const messaging::pricing_model_change& change,
                                      const ores::utility::domain::change_intent& intent,
                                      domain::pricing_model& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = pricing_model_to_domain(change.write);
    // The write lands under the acting party, so its claim is checked against
    // that party's row, not another visible party's row of the same key.
    const auto current =
        of_acting_party(pricing_model_read_one(repo_, ctx_, pricing_model_key_from(out)), ctx_);
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "pricing_model", .field = "code"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "pricing_model"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "pricing_model",
                               .field = "code",
                               .expected = change.precondition.version ?
                                               std::to_string(*change.precondition.version) :
                                               std::string{},
                               .current = std::to_string(current.front().version)});
            }
            break;
        case precondition_kind::any:
            break;
    }
    // The version is the repository's to state, from the claim: it is the one
    // thing the store's arbiter reads, and stating it in two places is how the
    // two come to disagree.
    stamp(out,
          ctx_,
          intent.reason_code.empty() ?
              std::string(ores::service::messaging::change_reasons::new_record) :
              intent.reason_code);
    out.change_commentary = intent.commentary;
    return result;
}


std::vector<domain::pricing_model> pricing_model_service::list_models(std::uint32_t offset,
                                                                      std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all pricing models";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t pricing_model_service::count_models() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total pricing models count";
    return repo_.get_total_model_count(ctx_);
}


std::optional<domain::pricing_model>
pricing_model_service::get_model_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting pricing model at version. " << "code: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::pricing_model> pricing_model_service::find_model(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding pricing model. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::pricing_model>
pricing_model_service::get_models(const std::vector<std::string>& codes) {
    return repo_.read_latest(ctx_, codes);
}

void pricing_model_service::save_model(const domain::pricing_model& v) {
    if (v.code.empty())
        throw std::invalid_argument("Pricing Model code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving pricing model. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved pricing model. " << "code: " << v.code;
}

void pricing_model_service::save_models(const std::vector<domain::pricing_model>& models) {
    for (const auto& e : models) {
        if (e.code.empty())
            throw std::invalid_argument("Pricing Model code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << models.size() << " pricing models";
    auto ts = models;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void pricing_model_service::delete_model(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing pricing model. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed pricing model. " << "code: " << code;
}

void pricing_model_service::delete_models(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::pricing_model>
pricing_model_service::get_model_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for pricing model. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
