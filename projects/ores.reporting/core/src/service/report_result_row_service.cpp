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
#include "ores.reporting.core/service/report_result_row_service.hpp"
#include "ores.reporting.api/domain/report_result_row.hpp"
#include "ores.reporting.api/messaging/report_result_row_protocol.hpp"
#include "ores.reporting.core/repository/report_result_row_repository.hpp"
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
#include "ores.database/repository/valid_at.hpp"
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

namespace ores::reporting::service {

using namespace ores::logging;

report_result_row_service::report_result_row_service(context ctx)
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
std::vector<domain::report_result_row>
report_result_row_read_one(repository::report_result_row_repository& repo,
                           const ores::database::context& ctx,
                           const messaging::report_result_row_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::report_result_row_key report_result_row_key_from(const domain::report_result_row& v) {
    messaging::report_result_row_key key;
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
domain::report_result_row
report_result_row_to_domain(const messaging::report_result_row_write& write) {
    domain::report_result_row v;
    v.id = write.id;
    v.party_id = write.party_id;
    v.report_instance_id = write.report_instance_id;
    v.as_of = write.as_of;
    v.subject_type = write.subject_type;
    v.subject_id = write.subject_id;
    v.measure = write.measure;
    v.currency = write.currency;
    v.report_definition_id = write.report_definition_id;
    v.counterparty_id = write.counterparty_id;
    v.figure = write.figure;
    v.source = write.source;
    return v;
}


}

messaging::list_report_result_rows_response report_result_row_service::list_report_result_rows(
    const messaging::list_report_result_rows_request& request) {
    messaging::list_report_result_rows_response response;
    if (!request.order.field.empty() &&
        !repository::report_result_row_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "report result rows", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->report_instance_id_one_of &&
        request.filter->report_instance_id_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "report_instance_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->measure_one_of &&
        request.filter->measure_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "measure_one_of", .limit = "1000"});
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
    response.rows = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_row_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_report_result_row_response report_result_row_service::get_report_result_row(
    const messaging::get_report_result_row_request& request) {
    messaging::get_report_result_row_response response;
    auto found = report_result_row_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "report_result_row"});
        return response;
    }
    response.report_result_row = std::move(found.front());
    return response;
}

messaging::get_many_report_result_rows_response
report_result_row_service::get_many_report_result_rows(
    const messaging::get_many_report_result_rows_request& request) {
    messaging::get_many_report_result_rows_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::report_result_row_lookup entry;
        entry.key = k;
        auto found = report_result_row_read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.report_result_row = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_report_result_row_response report_result_row_service::put_report_result_row(
    const messaging::put_report_result_row_request& request) {
    messaging::put_report_result_row_response response;
    domain::report_result_row value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = report_result_row_read_one(repo_, ctx_, report_result_row_key_from(value));
    if (!written.empty())
        response.report_result_row = std::move(written.front());
    return response;
}

messaging::put_many_report_result_rows_response
report_result_row_service::put_many_report_result_rows(
    const messaging::put_many_report_result_rows_request& request) {
    messaging::put_many_report_result_rows_response response;
    std::vector<domain::report_result_row> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::report_result_row value;
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
    response.rows.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = report_result_row_read_one(repo_, ctx_, report_result_row_key_from(value));
        response.rows.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_report_result_row_response report_result_row_service::delete_report_result_row(
    const messaging::delete_report_result_row_request& request) {
    messaging::delete_report_result_row_response response;
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
    switch (repo_.remove(ctx_, boost::uuids::to_string(request.removal.key.id), expected)) {
        case repository::report_result_row_repository::remove_status::removed:
            break;
        case repository::report_result_row_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "report_result_row"});
            break;
        case repository::report_result_row_repository::remove_status::conflicting: {
            // A resource that keeps no version cannot reach this status, so there
            // is no current version for its sentence to state.
            response.result =
                refuse(outcome_code::version_conflict,
                       {.entity = "report_result_row",
                        .field = "id",
                        .expected = expected ? std::to_string(*expected) : std::string{}});
            break;
        }
        case repository::report_result_row_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_report_result_rows_response
report_result_row_service::delete_many_report_result_rows(
    const messaging::delete_many_report_result_rows_request& request) {
    messaging::delete_many_report_result_rows_response response;
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
    std::vector<std::string> id_keys;
    id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        id_keys.push_back(boost::uuids::to_string(removal.key.id));
    repo_.remove(ctx_, id_keys);
    return response;
}

ores::utility::domain::result
report_result_row_service::prepare_change(const messaging::report_result_row_change& change,
                                          const ores::utility::domain::change_intent& intent,
                                          domain::report_result_row& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = report_result_row_to_domain(change.write);
    const auto current = report_result_row_read_one(repo_, ctx_, report_result_row_key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "report_result_row", .field = "id"});
            break;
        case precondition_kind::must_match_version:
            return refuse(outcome_code::precondition_not_supported);
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


std::vector<domain::report_result_row> report_result_row_service::list_rows(std::uint32_t offset,
                                                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all report result rows";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t report_result_row_service::count_rows() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total report result rows count";
    return repo_.get_total_row_count(ctx_);
}


std::optional<domain::report_result_row>
report_result_row_service::get_row(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting report result row. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::report_result_row>
report_result_row_service::get_rows(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void report_result_row_service::save_row(const domain::report_result_row& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Report result row id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving report result row. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved report result row. " << "id: " << v.id;
}

void report_result_row_service::save_rows(const std::vector<domain::report_result_row>& rows) {
    for (const auto& e : rows) {
        if (e.id.is_nil())
            throw std::invalid_argument("Report result row id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << rows.size() << " report result rows";
    auto ts = rows;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void report_result_row_service::delete_row(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing report result row. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed report result row. " << "id: " << id;
}

void report_result_row_service::delete_rows(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::report_result_row>
report_result_row_service::get_row_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for report result row. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
