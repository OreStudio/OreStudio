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
#include "ores.iam.core/service/login_info_service.hpp"
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

namespace ores::iam::service {

using namespace ores::logging;

login_info_service::login_info_service(context ctx)
    : ctx_(std::move(ctx)) {}
namespace {

/**
 * @brief The current row a key names, or an empty vector when there is none.
 *
 * A key record carries each column with the column's own type, and the
 * repository takes the text form every one of its key parameters shares, so
 * the conversion lives here rather than at every call site.
 */
std::vector<domain::login_info> read_one(repository::login_info_repository& repo,
                                         const ores::database::context& ctx,
                                         const messaging::login_info_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.account_id));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::login_info_key key_from(const domain::login_info& v) {
    messaging::login_info_key key;
    key.account_id = v.account_id;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::login_info to_domain(const messaging::login_info_write& write) {
    domain::login_info v;
    v.account_id = write.account_id;
    v.last_ip = write.last_ip;
    v.last_attempt_ip = write.last_attempt_ip;
    v.failed_logins = write.failed_logins;
    v.locked = write.locked;
    v.last_login = write.last_login;
    v.online = write.online;
    v.password_reset_required = write.password_reset_required;
    return v;
}

} // namespace

messaging::list_login_info_response
login_info_service::list_login_info(const messaging::list_login_info_request& request) {
    messaging::list_login_info_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    response.login_info = repo_.read_latest(ctx_, request.offset, request.limit);
    response.total = repo_.get_total_login_info_count(ctx_);
    return response;
}

messaging::get_login_info_response
login_info_service::get_login_info(const messaging::get_login_info_request& request) {
    messaging::get_login_info_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.login_info = std::move(found.front());
    return response;
}

messaging::get_many_login_info_response
login_info_service::get_many_login_info(const messaging::get_many_login_info_request& request) {
    messaging::get_many_login_info_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::login_info_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.login_info = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_login_info_response
login_info_service::put_login_info(const messaging::put_login_info_request& request) {
    messaging::put_login_info_response response;
    domain::login_info value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.login_info = std::move(written.front());
    return response;
}

messaging::put_many_login_info_response
login_info_service::put_many_login_info(const messaging::put_many_login_info_request& request) {
    messaging::put_many_login_info_response response;
    std::vector<domain::login_info> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::login_info value;
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
    response.login_info.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.login_info.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_login_info_response
login_info_service::delete_login_info(const messaging::delete_login_info_request& request) {
    messaging::delete_login_info_response response;
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
    switch (repo_.remove(ctx_, boost::uuids::to_string(request.removal.key.account_id), expected)) {
        case repository::login_info_repository::remove_status::removed:
            break;
        case repository::login_info_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::login_info_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::login_info_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_login_info_response login_info_service::delete_many_login_info(
    const messaging::delete_many_login_info_request& request) {
    messaging::delete_many_login_info_response response;
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
    std::vector<std::string> account_id_keys;
    account_id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        account_id_keys.push_back(boost::uuids::to_string(removal.key.account_id));
    repo_.remove(ctx_, account_id_keys);
    return response;
}

ores::utility::domain::result
login_info_service::prepare_change(const messaging::login_info_change& change,
                                   const ores::utility::domain::change_intent& intent,
                                   domain::login_info& out) {
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


std::vector<domain::login_info> login_info_service::list_login_info(std::uint32_t offset,
                                                                    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all login info";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t login_info_service::count_login_info() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total login info count";
    return repo_.get_total_login_info_count(ctx_);
}


std::optional<domain::login_info>
login_info_service::get_login_info(const std::string& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting login info. " << "account_id: " << account_id;
    auto results = repo_.read_latest(ctx_, account_id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::login_info>
login_info_service::get_login_info(const std::vector<std::string>& account_ids) {
    return repo_.read_latest(ctx_, account_ids);
}

void login_info_service::save_login_info(const domain::login_info& v) {
    if (v.account_id.is_nil())
        throw std::invalid_argument("Login Info account_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving login info. " << "account_id: " << v.account_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved login info. " << "account_id: " << v.account_id;
}

void login_info_service::save_login_info(const std::vector<domain::login_info>& login_info) {
    for (const auto& e : login_info) {
        if (e.account_id.is_nil())
            throw std::invalid_argument("Login Info account_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << login_info.size() << " login info";
    auto ts = login_info;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void login_info_service::delete_login_info(const std::string& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing login info. " << "account_id: " << account_id;
    repo_.remove(ctx_, account_id);
    BOOST_LOG_SEV(lg(), info) << "Removed login info. " << "account_id: " << account_id;
}

void login_info_service::delete_login_info(const std::vector<std::string>& account_ids) {
    repo_.remove(ctx_, account_ids);
}


}
