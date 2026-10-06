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
#include "ores.refdata.core/service/portfolio_right_service.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

portfolio_right_service::portfolio_right_service(context ctx)
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
std::vector<domain::portfolio_right> read_one(repository::portfolio_right_repository& repo,
                                              const ores::database::context& ctx,
                                              const messaging::portfolio_right_key& key) {
    return repo.read_latest_by_right_code(ctx, key.right_code);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::portfolio_right_key key_from(const domain::portfolio_right& v) {
    messaging::portfolio_right_key key;
    key.right_code = v.right_code;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::portfolio_right to_domain(const messaging::portfolio_right_write& write) {
    domain::portfolio_right v;
    v.id = write.id;
    v.account_id = write.account_id;
    v.portfolio_id = write.portfolio_id;
    v.right_code = write.right_code;
    return v;
}

}

messaging::list_portfolio_rights_response portfolio_right_service::list_portfolio_rights(
    const messaging::list_portfolio_rights_request& request) {
    messaging::list_portfolio_rights_response response;
    if (!request.order.field.empty() &&
        !repository::portfolio_right_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of portfolio rights cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_one_of.";
        return response;
    }
    if (request.filter && request.filter->account_id_one_of &&
        request.filter->account_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in account_id_one_of.";
        return response;
    }
    if (request.filter && request.filter->portfolio_id_one_of &&
        request.filter->portfolio_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in portfolio_id_one_of.";
        return response;
    }
    // A stated instant is parsed here, so a malformed one is the caller's
    // mistake rather than a database error, and the store reads one form.
    std::optional<std::string> as_of;
    if (request.as_of) {
        try {
            as_of = ores::platform::time::datetime::to_iso8601_utc(
                ores::platform::time::datetime::from_iso8601_utc(*request.as_of));
        } catch (const std::invalid_argument&) {
            response.result.outcome = ores::utility::domain::outcome::invalid;
            response.result.code = "as_of_invalid";
            response.result.message = "as_of is not a UTC timestamp: " + *request.as_of;
            return response;
        }
    }
    response.portfolio_rights = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_portfolio_right_count(ctx_, request.filter, as_of);
    return response;
}

messaging::list_by_account_id_portfolio_rights_response
portfolio_right_service::list_by_account_id_portfolio_rights(
    const messaging::list_by_account_id_portfolio_rights_request& request) {
    messaging::list_by_account_id_portfolio_rights_response response;
    if (!request.order.field.empty() &&
        !repository::portfolio_right_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of portfolio rights cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_one_of.";
        return response;
    }
    if (request.filter && request.filter->account_id_one_of &&
        request.filter->account_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in account_id_one_of.";
        return response;
    }
    if (request.filter && request.filter->portfolio_id_one_of &&
        request.filter->portfolio_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in portfolio_id_one_of.";
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "scope_not_supported";
        response.result.message = "This resource reads its direct members; it has no subtree.";
        return response;
    }
    const auto relation = boost::uuids::to_string(request.account_id);
    response.portfolio_rights = repo_.read_latest_by_account_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total =
        repo_.get_total_portfolio_right_count_by_account_id(ctx_, relation, request.filter);
    return response;
}

messaging::list_by_portfolio_id_portfolio_rights_response
portfolio_right_service::list_by_portfolio_id_portfolio_rights(
    const messaging::list_by_portfolio_id_portfolio_rights_request& request) {
    messaging::list_by_portfolio_id_portfolio_rights_response response;
    if (!request.order.field.empty() &&
        !repository::portfolio_right_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of portfolio rights cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_one_of.";
        return response;
    }
    if (request.filter && request.filter->account_id_one_of &&
        request.filter->account_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in account_id_one_of.";
        return response;
    }
    if (request.filter && request.filter->portfolio_id_one_of &&
        request.filter->portfolio_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in portfolio_id_one_of.";
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "scope_not_supported";
        response.result.message = "This resource reads its direct members; it has no subtree.";
        return response;
    }
    const auto relation = boost::uuids::to_string(request.portfolio_id);
    response.portfolio_rights = repo_.read_latest_by_portfolio_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total =
        repo_.get_total_portfolio_right_count_by_portfolio_id(ctx_, relation, request.filter);
    return response;
}

messaging::get_portfolio_right_response portfolio_right_service::get_portfolio_right(
    const messaging::get_portfolio_right_request& request) {
    messaging::get_portfolio_right_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.portfolio_right = std::move(found.front());
    return response;
}

messaging::get_many_portfolio_rights_response portfolio_right_service::get_many_portfolio_rights(
    const messaging::get_many_portfolio_rights_request& request) {
    messaging::get_many_portfolio_rights_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::portfolio_right_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.portfolio_right = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_portfolio_right_response portfolio_right_service::put_portfolio_right(
    const messaging::put_portfolio_right_request& request) {
    messaging::put_portfolio_right_response response;
    domain::portfolio_right value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.portfolio_right = std::move(written.front());
    return response;
}

messaging::put_many_portfolio_rights_response portfolio_right_service::put_many_portfolio_rights(
    const messaging::put_many_portfolio_rights_request& request) {
    messaging::put_many_portfolio_rights_response response;
    std::vector<domain::portfolio_right> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::portfolio_right value;
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
    response.portfolio_rights.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.portfolio_rights.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_portfolio_right_response portfolio_right_service::delete_portfolio_right(
    const messaging::delete_portfolio_right_request& request) {
    messaging::delete_portfolio_right_response response;
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
    const auto named = read_one(repo_, ctx_, request.removal.key);
    if (named.empty()) {
        response.result.outcome = outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    const auto& row = named.front();
    switch (repo_.remove(ctx_, boost::uuids::to_string(row.id), expected)) {
        case repository::portfolio_right_repository::remove_status::removed:
            break;
        case repository::portfolio_right_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::portfolio_right_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::portfolio_right_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_portfolio_rights_response
portfolio_right_service::delete_many_portfolio_rights(
    const messaging::delete_many_portfolio_rights_request& request) {
    messaging::delete_many_portfolio_rights_response response;
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
    // A removal names its row by the key a caller holds, and the repository
    // takes the storage key, so the two are joined once here rather than at
    // each column's conversion. A name that matches no row is skipped: the
    // batch reports what it removed, and a row that is already gone is not a
    // failure.
    std::vector<domain::portfolio_right> resolved;
    resolved.reserve(request.removals.size());
    for (const auto& removal : request.removals) {
        auto named = read_one(repo_, ctx_, removal.key);
        if (!named.empty())
            resolved.push_back(std::move(named.front()));
    }
    std::vector<std::string> id_keys;
    id_keys.reserve(resolved.size());
    for (const auto& row : resolved)
        id_keys.push_back(boost::uuids::to_string(row.id));
    repo_.remove(ctx_, id_keys);
    return response;
}

messaging::list_portfolio_right_versions_response
portfolio_right_service::list_portfolio_right_versions(
    const messaging::list_portfolio_right_versions_request& request) {
    messaging::list_portfolio_right_versions_response response;
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
    // The versions of the row the caller's key names. The repository reads by
    // the storage key, so the declared key is resolved once here.
    const auto named = read_one(repo_, ctx_, request.key);
    if (named.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    const auto& row = named.front();
    auto all = repo_.read_all(ctx_, boost::uuids::to_string(row.id));
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

messaging::get_portfolio_right_version_response
portfolio_right_service::get_portfolio_right_version(
    const messaging::get_portfolio_right_version_request& request) {
    messaging::get_portfolio_right_version_response response;
    // The version key nests the entity's own key, which is the declared one.
    // The repository reads by the storage key, so it is resolved once here.
    const auto named = read_one(repo_, ctx_, request.key.portfolio_right);
    if (named.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    const auto& row = named.front();
    auto found = repo_.read_at_version(ctx_, boost::uuids::to_string(row.id), request.key.version);
    if (!found) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
portfolio_right_service::prepare_change(const messaging::portfolio_right_change& change,
                                        const ores::utility::domain::change_intent& intent,
                                        domain::portfolio_right& out) {
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


std::vector<domain::portfolio_right>
portfolio_right_service::list_portfolio_rights(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all portfolio rights";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t portfolio_right_service::count_portfolio_rights() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total portfolio rights count";
    return repo_.get_total_portfolio_right_count(ctx_);
}


std::vector<domain::portfolio_right> portfolio_right_service::list_portfolio_rights_by_account_id(
    const std::string& account_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing portfolio rights by account_id: " << account_id;
    return repo_.read_latest_by_account_id(ctx_, account_id, offset, limit);
}

std::uint32_t
portfolio_right_service::count_portfolio_rights_by_account_id(const std::string& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total portfolio rights count by account_id: "
                               << account_id;
    return repo_.get_total_portfolio_right_count_by_account_id(ctx_, account_id);
}

std::vector<domain::portfolio_right> portfolio_right_service::list_portfolio_rights_by_account_id(
    const boost::uuids::uuid& account_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing portfolio rights by account_id: " << account_id;
    return repo_.read_latest_by_account_id(
        ctx_, boost::uuids::to_string(account_id), offset, limit);
}

std::uint32_t portfolio_right_service::count_portfolio_rights_by_account_id(
    const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total portfolio rights count by account_id: "
                               << account_id;
    return repo_.get_total_portfolio_right_count_by_account_id(ctx_,
                                                               boost::uuids::to_string(account_id));
}


std::vector<domain::portfolio_right> portfolio_right_service::list_portfolio_rights_by_portfolio_id(
    const std::string& portfolio_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing portfolio rights by portfolio_id: " << portfolio_id;
    return repo_.read_latest_by_portfolio_id(ctx_, portfolio_id, offset, limit);
}

std::uint32_t
portfolio_right_service::count_portfolio_rights_by_portfolio_id(const std::string& portfolio_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total portfolio rights count by portfolio_id: "
                               << portfolio_id;
    return repo_.get_total_portfolio_right_count_by_portfolio_id(ctx_, portfolio_id);
}

std::vector<domain::portfolio_right> portfolio_right_service::list_portfolio_rights_by_portfolio_id(
    const boost::uuids::uuid& portfolio_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing portfolio rights by portfolio_id: " << portfolio_id;
    return repo_.read_latest_by_portfolio_id(
        ctx_, boost::uuids::to_string(portfolio_id), offset, limit);
}

std::uint32_t portfolio_right_service::count_portfolio_rights_by_portfolio_id(
    const boost::uuids::uuid& portfolio_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total portfolio rights count by portfolio_id: "
                               << portfolio_id;
    return repo_.get_total_portfolio_right_count_by_portfolio_id(
        ctx_, boost::uuids::to_string(portfolio_id));
}


std::optional<domain::portfolio_right>
portfolio_right_service::get_portfolio_right_at_version(const boost::uuids::uuid& id,
                                                        std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting portfolio right at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::portfolio_right>
portfolio_right_service::get_portfolio_right(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting portfolio right. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::optional<domain::portfolio_right>
portfolio_right_service::get_portfolio_right_by_right_code(const std::string& right_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting portfolio right by right_code: " << right_code;
    messaging::portfolio_right_key k;
    k.right_code = right_code;
    auto found = read_one(repo_, ctx_, k);
    if (found.empty())
        return std::nullopt;
    return found.front();
}

std::optional<domain::portfolio_right>
portfolio_right_service::find_portfolio_right(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding portfolio right. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::portfolio_right>
portfolio_right_service::get_portfolio_rights(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void portfolio_right_service::save_portfolio_right(const domain::portfolio_right& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Portfolio Right id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving portfolio right. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved portfolio right. " << "id: " << v.id;
}

void portfolio_right_service::save_portfolio_rights(
    const std::vector<domain::portfolio_right>& portfolio_rights) {
    for (const auto& e : portfolio_rights) {
        if (e.id.is_nil())
            throw std::invalid_argument("Portfolio Right id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << portfolio_rights.size() << " portfolio rights";
    auto ts = portfolio_rights;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void portfolio_right_service::delete_portfolio_right(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing portfolio right. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed portfolio right. " << "id: " << id;
}

void portfolio_right_service::remove_portfolio_right(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing portfolio right. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed portfolio right. " << "id: " << id;
}

void portfolio_right_service::delete_portfolio_rights(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::portfolio_right>
portfolio_right_service::get_portfolio_right_history(const std::string& key) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for portfolio right. key: " << key;
    // The caller holds the key the model declares and this reads by the
    // storage key, so the two are joined here exactly as they are for any
    // other read. Without this step a provider looks the versions up under a
    // value the storage key never holds, and reports an entity that has a
    // history as having none.
    messaging::portfolio_right_key k;
    k.right_code = key;
    // A delete here closes the transaction-time window and leaves every version
    // in place, so resolving through a latest read would lose the history at
    // exactly the moment it is wanted. This takes the newest row carrying the
    // declared key whether or not it is still current, which for a record that
    // still exists is the same row the latest read would have returned.
    const auto found = repo_.read_any_by_right_code(ctx_, k.right_code);
    if (found.empty())
        return {};
    const auto& row = found.front();
    return repo_.read_all(ctx_, boost::uuids::to_string(row.id));
}

std::vector<domain::portfolio_right>
portfolio_right_service::get_portfolio_right_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for portfolio right. " << "id: " << id;
    return repo_.read_all(ctx_, boost::uuids::to_string(id));
}

}
