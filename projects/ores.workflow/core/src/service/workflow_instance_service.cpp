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
#include "ores.workflow.core/service/workflow_instance_service.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <algorithm>
#include <cstdint>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using ores::service::messaging::stamp;

namespace ores::workflow::service {

using namespace ores::logging;

workflow_instance_service::workflow_instance_service(context ctx)
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
std::vector<domain::workflow_instance> read_one(repository::workflow_instance_repository& repo,
                                                const ores::database::context& ctx,
                                                const messaging::workflow_instance_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::workflow_instance_key key_from(const domain::workflow_instance& v) {
    messaging::workflow_instance_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::workflow_instance to_domain(const messaging::workflow_instance_write& write) {
    domain::workflow_instance v;
    v.id = write.id;
    v.type = write.type;
    v.state_id = write.state_id;
    v.request_json = write.request_json;
    v.result_json = write.result_json;
    v.error = write.error;
    v.correlation_id = write.correlation_id;
    v.created_by = write.created_by;
    v.current_step_index = write.current_step_index;
    v.step_count = write.step_count;
    v.materialised_steps_json = write.materialised_steps_json;
    v.completed_at = write.completed_at;
    v.last_event_at = write.last_event_at;
    return v;
}

} // namespace

messaging::list_workflow_instances_response workflow_instance_service::list_workflow_instances(
    const messaging::list_workflow_instances_request& request) {
    messaging::list_workflow_instances_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    response.instances = repo_.read_latest(ctx_, request.offset, request.limit);
    response.total = repo_.get_total_instance_count(ctx_);
    return response;
}

messaging::get_workflow_instance_response workflow_instance_service::get_workflow_instance(
    const messaging::get_workflow_instance_request& request) {
    messaging::get_workflow_instance_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.workflow_instance = std::move(found.front());
    return response;
}

messaging::get_many_workflow_instances_response
workflow_instance_service::get_many_workflow_instances(
    const messaging::get_many_workflow_instances_request& request) {
    messaging::get_many_workflow_instances_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::workflow_instance_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.workflow_instance = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_workflow_instance_response workflow_instance_service::put_workflow_instance(
    const messaging::put_workflow_instance_request& request) {
    messaging::put_workflow_instance_response response;
    domain::workflow_instance value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.workflow_instance = std::move(written.front());
    return response;
}

messaging::put_many_workflow_instances_response
workflow_instance_service::put_many_workflow_instances(
    const messaging::put_many_workflow_instances_request& request) {
    messaging::put_many_workflow_instances_response response;
    std::vector<domain::workflow_instance> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::workflow_instance value;
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
    response.instances.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.instances.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_workflow_instance_response workflow_instance_service::delete_workflow_instance(
    const messaging::delete_workflow_instance_request& request) {
    messaging::delete_workflow_instance_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    if (request.removal.precondition.kind == precondition_kind::must_not_exist) {
        response.result.outcome = outcome::invalid;
        response.result.code = "precondition_not_supported";
        response.result.message = "A removal cannot require that a row is absent.";
        return response;
    }
    std::optional<std::uint32_t> expected;
    if (request.removal.precondition.kind == precondition_kind::must_match_version) {
        if (!request.removal.precondition.version) {
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_incomplete";
            response.result.message = "A versioned removal must state the version it expects.";
            return response;
        }
        expected = request.removal.precondition.version;
    }
    switch (repo_.remove(ctx_, boost::uuids::to_string(request.removal.key.id), expected)) {
        case repository::workflow_instance_repository::remove_status::removed:
            break;
        case repository::workflow_instance_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::workflow_instance_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::workflow_instance_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_workflow_instances_response
workflow_instance_service::delete_many_workflow_instances(
    const messaging::delete_many_workflow_instances_request& request) {
    messaging::delete_many_workflow_instances_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    for (const auto& removal : request.removals) {
        if (removal.precondition.kind != precondition_kind::any) {
            // The store removes a set in one statement, which carries no
            // per-row version. Refusing is the only answer that keeps the
            // batch atomic: serving it as a sequence of single removals would
            // leave a partial batch behind as soon as one row had moved on.
            response.result.outcome = outcome::invalid;
            response.result.code = "batch_removal_is_unconditional";
            response.result.message =
                "A batch removal is unconditional; remove the rows one at a time "
                "to state a version.";
            return response;
        }
    }
    if (request.removals.empty())
        return response;
    std::vector<std::string> id_keys;
    id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        id_keys.push_back(boost::uuids::to_string(removal.key.id));
    repo_.remove(ctx_, id_keys);
    return response;
}

messaging::list_workflow_instance_versions_response
workflow_instance_service::list_workflow_instance_versions(
    const messaging::list_workflow_instance_versions_request& request) {
    messaging::list_workflow_instance_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    if (request.filter) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_not_supported";
        response.result.message = "Filtering is not served for this resource yet.";
        return response;
    }
    auto all = repo_.read_all(ctx_, boost::uuids::to_string(request.key.id));
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

messaging::get_workflow_instance_version_response
workflow_instance_service::get_workflow_instance_version(
    const messaging::get_workflow_instance_version_request& request) {
    messaging::get_workflow_instance_version_response response;
    auto found = repo_.read_at_version(
        ctx_, boost::uuids::to_string(request.key.workflow_instance.id), request.key.version);
    if (!found) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
workflow_instance_service::prepare_change(const messaging::workflow_instance_change& change,
                                          const ores::utility::domain::change_intent& intent,
                                          domain::workflow_instance& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, ctx_, key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty()) {
                result.outcome = outcome::conflict;
                result.code = "already_exists";
                return result;
            }
            break;
        case precondition_kind::must_match_version:
            if (current.empty()) {
                result.outcome = outcome::missing;
                result.code = "not_found";
                return result;
            }
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                result.outcome = outcome::conflict;
                result.code = "version_conflict";
                return result;
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


std::vector<domain::workflow_instance>
workflow_instance_service::list_instances(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all workflow instances";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t workflow_instance_service::count_instances() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total workflow instances count";
    return repo_.get_total_instance_count(ctx_);
}


std::optional<domain::workflow_instance>
workflow_instance_service::get_instance_at_version(const boost::uuids::uuid& id,
                                                   std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting workflow instance at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::workflow_instance>
workflow_instance_service::get_instance(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting workflow instance. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::workflow_instance>
workflow_instance_service::get_instances(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void workflow_instance_service::save_instance(const domain::workflow_instance& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Workflow Instance id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving workflow instance. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved workflow instance. " << "id: " << v.id;
}

void workflow_instance_service::save_instances(
    const std::vector<domain::workflow_instance>& instances) {
    for (const auto& e : instances) {
        if (e.id.is_nil())
            throw std::invalid_argument("Workflow Instance id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << instances.size() << " workflow instances";
    auto ts = instances;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void workflow_instance_service::delete_instance(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing workflow instance. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed workflow instance. " << "id: " << id;
}

void workflow_instance_service::delete_instances(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::workflow_instance>
workflow_instance_service::get_instance_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for workflow instance. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
