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
#include "ores.inbox.core/service/notification_recipient_service.hpp"
#include "ores.inbox.api/domain/notification_recipient.hpp"
#include "ores.inbox.api/messaging/notification_recipient_protocol.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <vector>
// Log lines stream uuids with uuid_io's operator<<, which the include check
// does not count as a use.
#include "ores.database/domain/outcome_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp> // IWYU pragma: keep.

namespace ores::inbox::service {

using namespace ores::logging;
using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;


notification_recipient_service::notification_recipient_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_(ctx_) {}

namespace {

/**
 * @brief The current row a key names, or an empty vector when there is none.
 *
 * A junction key is the pair of columns that names one link, and the
 * repository takes each in its own column's type, so the pair passes
 * straight through.
 */
std::vector<domain::notification_recipient>
read_one(repository::notification_recipient_repository& repo,
         const messaging::notification_recipient_key& key) {
    return repo.read_latest(key.notification_id, key.account_id);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::notification_recipient_key key_from(const domain::notification_recipient& v) {
    messaging::notification_recipient_key key;
    key.notification_id = v.notification_id;
    key.account_id = v.account_id;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the two halves of the key and the junction's own
 * columns and nothing else: tenancy, provenance, the version and the
 * validity window are the service's and the database's to state, and are
 * set after this conversion.
 */
domain::notification_recipient to_domain(const messaging::notification_recipient_write& write) {
    domain::notification_recipient v;
    v.notification_id = write.notification_id;
    v.account_id = write.account_id;
    v.read_at = write.read_at;
    v.cleared_at = write.cleared_at;
    return v;
}

}

messaging::list_notification_recipients_response
notification_recipient_service::list_notification_recipients(
    const messaging::list_notification_recipients_request& request) {
    messaging::list_notification_recipients_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "notification_recipients", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "notification_recipients"});
        return response;
    }
    response.notification_recipients = repo_.read_latest(request.offset, request.limit);
    response.total = repo_.get_total_recipient_count();
    return response;
}

messaging::list_by_account_id_notification_recipients_response
notification_recipient_service::list_by_account_id_notification_recipients(
    const messaging::list_by_account_id_notification_recipients_request& request) {
    messaging::list_by_account_id_notification_recipients_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "notification_recipients", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "notification_recipients"});
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result =
            refuse(outcome_code::scope_not_supported, {.entity = "notification_recipients"});
        return response;
    }
    response.notification_recipients =
        repo_.read_latest_by_account(request.account_id, request.offset, request.limit);
    response.total = repo_.get_total_recipient_count_by_account(request.account_id);
    return response;
}

messaging::get_notification_recipient_response
notification_recipient_service::get_notification_recipient(
    const messaging::get_notification_recipient_request& request) {
    messaging::get_notification_recipient_response response;
    auto found = read_one(repo_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "notification_recipient"});
        return response;
    }
    response.notification_recipient = std::move(found.front());
    return response;
}

messaging::get_many_notification_recipients_response
notification_recipient_service::get_many_notification_recipients(
    const messaging::get_many_notification_recipients_request& request) {
    messaging::get_many_notification_recipients_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::notification_recipient_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, k);
        if (!found.empty())
            entry.notification_recipient = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_notification_recipient_response
notification_recipient_service::put_notification_recipient(
    const messaging::put_notification_recipient_request& request) {
    messaging::put_notification_recipient_response response;
    domain::notification_recipient value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(value, request.change.precondition);
    auto written = read_one(repo_, key_from(value));
    if (!written.empty())
        response.notification_recipient = std::move(written.front());
    return response;
}

messaging::put_many_notification_recipients_response
notification_recipient_service::put_many_notification_recipients(
    const messaging::put_many_notification_recipients_request& request) {
    messaging::put_many_notification_recipients_response response;
    std::vector<domain::notification_recipient> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::notification_recipient value;
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
    repo_.write(batch, claims);
    response.notification_recipients.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, key_from(value));
        response.notification_recipients.push_back(written.empty() ? value :
                                                                     std::move(written.front()));
    }
    return response;
}

messaging::delete_notification_recipient_response
notification_recipient_service::delete_notification_recipient(
    const messaging::delete_notification_recipient_request& request) {
    messaging::delete_notification_recipient_response response;
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
    switch (repo_.remove(
        request.removal.key.notification_id, request.removal.key.account_id, expected)) {
        case repository::notification_recipient_repository::remove_status::removed:
            break;
        case repository::notification_recipient_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "notification_recipient"});
            break;
        case repository::notification_recipient_repository::remove_status::conflicting: {
            // The junction's removal states neither version, so the row is read for
            // the one it now holds. The sentence carries what the caller stated and
            // what the store holds.
            const auto live = read_one(repo_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "notification_recipient",
                 .field = "notification_id",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::notification_recipient_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_notification_recipients_response
notification_recipient_service::delete_many_notification_recipients(
    const messaging::delete_many_notification_recipients_request& request) {
    messaging::delete_many_notification_recipients_response response;
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
    std::vector<boost::uuids::uuid> notification_id_keys;
    notification_id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        notification_id_keys.push_back(removal.key.notification_id);
    std::vector<boost::uuids::uuid> account_id_keys;
    account_id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        account_id_keys.push_back(removal.key.account_id);
    repo_.remove(notification_id_keys, account_id_keys);
    return response;
}

ores::utility::domain::result notification_recipient_service::prepare_change(
    const messaging::notification_recipient_change& change,
    const ores::utility::domain::change_intent& intent,
    domain::notification_recipient& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "notification_recipient", .field = "notification_id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "notification_recipient"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "notification_recipient",
                               .field = "notification_id",
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
    return result;
}

}
