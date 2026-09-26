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
#include "ores.dq.core/service/lei_entity_service.hpp"
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

lei_entity_service::lei_entity_service(context ctx)
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
std::vector<domain::lei_entity> read_one(repository::lei_entity_repository& repo,
                                         const ores::database::context& ctx,
                                         const messaging::lei_entity_key& key) {
    return repo.read_latest(ctx, key.lei);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::lei_entity_key key_from(const domain::lei_entity& v) {
    messaging::lei_entity_key key;
    key.lei = v.lei;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::lei_entity to_domain(const messaging::lei_entity_write& write) {
    domain::lei_entity v;
    v.lei = write.lei;
    v.entity_legal_name = write.entity_legal_name;
    v.entity_entity_category = write.entity_entity_category;
    v.entity_entity_sub_category = write.entity_entity_sub_category;
    v.entity_entity_status = write.entity_entity_status;
    v.entity_legal_form_entity_legal_form_code = write.entity_legal_form_entity_legal_form_code;
    v.entity_legal_form_other_legal_form = write.entity_legal_form_other_legal_form;
    v.entity_legal_jurisdiction = write.entity_legal_jurisdiction;
    v.entity_legal_address_first_address_line = write.entity_legal_address_first_address_line;
    v.entity_legal_address_city = write.entity_legal_address_city;
    v.entity_legal_address_region = write.entity_legal_address_region;
    v.entity_legal_address_country = write.entity_legal_address_country;
    v.entity_legal_address_postal_code = write.entity_legal_address_postal_code;
    v.entity_headquarters_address_first_address_line =
        write.entity_headquarters_address_first_address_line;
    v.entity_headquarters_address_city = write.entity_headquarters_address_city;
    v.entity_headquarters_address_region = write.entity_headquarters_address_region;
    v.entity_headquarters_address_country = write.entity_headquarters_address_country;
    v.entity_headquarters_address_postal_code = write.entity_headquarters_address_postal_code;
    v.entity_entity_creation_date = write.entity_entity_creation_date;
    v.registration_initial_registration_date = write.registration_initial_registration_date;
    v.registration_last_update_date = write.registration_last_update_date;
    v.registration_next_renewal_date = write.registration_next_renewal_date;
    v.registration_registration_status = write.registration_registration_status;
    v.entity_transliterated_name_1 = write.entity_transliterated_name_1;
    v.entity_transliterated_name_1_type = write.entity_transliterated_name_1_type;
    return v;
}

} // namespace

messaging::list_lei_entities_response
lei_entity_service::list_lei_entities(const messaging::list_lei_entities_request& request) {
    messaging::list_lei_entities_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    response.entities = repo_.read_latest(ctx_, request.offset, request.limit);
    response.total = repo_.get_total_entity_count(ctx_);
    return response;
}

messaging::get_lei_entity_response
lei_entity_service::get_lei_entity(const messaging::get_lei_entity_request& request) {
    messaging::get_lei_entity_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.lei_entity = std::move(found.front());
    return response;
}

messaging::get_many_lei_entities_response
lei_entity_service::get_many_lei_entities(const messaging::get_many_lei_entities_request& request) {
    messaging::get_many_lei_entities_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::lei_entity_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.lei_entity = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_lei_entity_response
lei_entity_service::put_lei_entity(const messaging::put_lei_entity_request& request) {
    messaging::put_lei_entity_response response;
    domain::lei_entity value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.lei_entity = std::move(written.front());
    return response;
}

messaging::put_many_lei_entities_response
lei_entity_service::put_many_lei_entities(const messaging::put_many_lei_entities_request& request) {
    messaging::put_many_lei_entities_response response;
    std::vector<domain::lei_entity> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::lei_entity value;
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
    response.entities.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.entities.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_lei_entity_response
lei_entity_service::delete_lei_entity(const messaging::delete_lei_entity_request& request) {
    messaging::delete_lei_entity_response response;
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
    switch (repo_.remove(ctx_, request.removal.key.lei, expected)) {
        case repository::lei_entity_repository::remove_status::removed:
            break;
        case repository::lei_entity_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::lei_entity_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::lei_entity_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_lei_entities_response lei_entity_service::delete_many_lei_entities(
    const messaging::delete_many_lei_entities_request& request) {
    messaging::delete_many_lei_entities_response response;
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
    std::vector<std::string> lei_keys;
    lei_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        lei_keys.push_back(removal.key.lei);
    repo_.remove(ctx_, lei_keys);
    return response;
}

messaging::list_lei_entity_versions_response lei_entity_service::list_lei_entity_versions(
    const messaging::list_lei_entity_versions_request& request) {
    messaging::list_lei_entity_versions_response response;
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
    auto all = repo_.read_all(ctx_, request.key.lei);
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

messaging::get_lei_entity_version_response lei_entity_service::get_lei_entity_version(
    const messaging::get_lei_entity_version_request& request) {
    messaging::get_lei_entity_version_response response;
    auto found = repo_.read_at_version(ctx_, request.key.lei_entity.lei, request.key.version);
    if (!found) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
lei_entity_service::prepare_change(const messaging::lei_entity_change& change,
                                   const ores::utility::domain::change_intent& intent,
                                   domain::lei_entity& out) {
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


std::vector<domain::lei_entity> lei_entity_service::list_entities(std::uint32_t offset,
                                                                  std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all LEI entities";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t lei_entity_service::count_entities() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total LEI entities count";
    return repo_.get_total_entity_count(ctx_);
}


std::optional<domain::lei_entity> lei_entity_service::get_entity_at_version(const std::string& lei,
                                                                            std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting LEI entity at version. " << "lei: " << lei
                               << " version: " << version;
    return repo_.read_at_version(ctx_, lei, version);
}

std::optional<domain::lei_entity> lei_entity_service::get_entity(const std::string& lei) {
    BOOST_LOG_SEV(lg(), debug) << "Getting LEI entity. " << "lei: " << lei;
    auto results = repo_.read_latest(ctx_, lei);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::lei_entity>
lei_entity_service::get_entities(const std::vector<std::string>& leis) {
    return repo_.read_latest(ctx_, leis);
}

void lei_entity_service::save_entity(const domain::lei_entity& v) {
    if (v.lei.empty())
        throw std::invalid_argument("LEI Entity lei cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving LEI entity. " << "lei: " << v.lei;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved LEI entity. " << "lei: " << v.lei;
}

void lei_entity_service::save_entities(const std::vector<domain::lei_entity>& entities) {
    for (const auto& e : entities) {
        if (e.lei.empty())
            throw std::invalid_argument("LEI Entity lei cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << entities.size() << " LEI entities";
    auto ts = entities;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void lei_entity_service::delete_entity(const std::string& lei) {
    BOOST_LOG_SEV(lg(), debug) << "Removing LEI entity. " << "lei: " << lei;
    repo_.remove(ctx_, lei);
    BOOST_LOG_SEV(lg(), info) << "Removed LEI entity. " << "lei: " << lei;
}

void lei_entity_service::delete_entities(const std::vector<std::string>& leis) {
    repo_.remove(ctx_, leis);
}

std::vector<domain::lei_entity> lei_entity_service::get_entity_history(const std::string& lei) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for LEI entity. " << "lei: " << lei;
    return repo_.read_all(ctx_, lei);
}

}
