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
#include "ores.dq.core/service/publication_service.hpp"
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

namespace ores::dq::service {

using namespace ores::logging;

publication_service::publication_service(context ctx)
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
std::vector<domain::publication> read_one(repository::publication_repository& repo,
                                          const ores::database::context& ctx,
                                          const messaging::publication_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::publication_key key_from(const domain::publication& v) {
    messaging::publication_key key;
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
domain::publication to_domain(const messaging::publication_write& write) {
    domain::publication v;
    v.id = write.id;
    v.dataset_id = write.dataset_id;
    v.dataset_code = write.dataset_code;
    v.mode = write.mode;
    v.target_table = write.target_table;
    v.records_inserted = write.records_inserted;
    v.records_updated = write.records_updated;
    v.records_skipped = write.records_skipped;
    v.records_deleted = write.records_deleted;
    v.published_by = write.published_by;
    v.published_at = write.published_at;
    return v;
}

} // namespace

messaging::list_publications_response
publication_service::list_publications(const messaging::list_publications_request& request) {
    messaging::list_publications_response response;
    if (!request.order.field.empty() &&
        !repository::publication_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of publications cannot be ordered by " + request.order.field + ".";
        return response;
    }
    response.publications = repo_.read_latest(ctx_, request.offset, request.limit, request.order);
    response.total = repo_.get_total_publication_count(ctx_);
    return response;
}

messaging::get_publication_response
publication_service::get_publication(const messaging::get_publication_request& request) {
    messaging::get_publication_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.publication = std::move(found.front());
    return response;
}

messaging::get_many_publications_response publication_service::get_many_publications(
    const messaging::get_many_publications_request& request) {
    messaging::get_many_publications_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::publication_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.publication = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_publication_response
publication_service::put_publication(const messaging::put_publication_request& request) {
    messaging::put_publication_response response;
    domain::publication value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.publication = std::move(written.front());
    return response;
}

messaging::put_many_publications_response publication_service::put_many_publications(
    const messaging::put_many_publications_request& request) {
    messaging::put_many_publications_response response;
    std::vector<domain::publication> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::publication value;
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
    response.publications.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.publications.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_publication_response
publication_service::delete_publication(const messaging::delete_publication_request& request) {
    messaging::delete_publication_response response;
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
        case repository::publication_repository::remove_status::removed:
            break;
        case repository::publication_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::publication_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::publication_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_publications_response publication_service::delete_many_publications(
    const messaging::delete_many_publications_request& request) {
    messaging::delete_many_publications_response response;
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

ores::utility::domain::result
publication_service::prepare_change(const messaging::publication_change& change,
                                    const ores::utility::domain::change_intent& intent,
                                    domain::publication& out) {
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
            result.outcome = outcome::invalid;
            result.code = "precondition_not_supported";
            result.message = "This resource keeps no version to match.";
            return result;
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
    return result;
}


std::vector<domain::publication> publication_service::list_publications(std::uint32_t offset,
                                                                        std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all publications";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t publication_service::count_publications() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total publications count";
    return repo_.get_total_publication_count(ctx_);
}


std::optional<domain::publication>
publication_service::get_publication(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting publication. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::publication>
publication_service::get_publications(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void publication_service::save_publication(const domain::publication& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Publication id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving publication. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved publication. " << "id: " << v.id;
}

void publication_service::save_publications(const std::vector<domain::publication>& publications) {
    for (const auto& e : publications) {
        if (e.id.is_nil())
            throw std::invalid_argument("Publication id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << publications.size() << " publications";
    auto ts = publications;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void publication_service::delete_publication(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing publication. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed publication. " << "id: " << id;
}

void publication_service::delete_publications(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}


}
