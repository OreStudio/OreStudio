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
#include "ores.refdata.core/service/bundle_import_operations_service.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/bundle_import.hpp"
#include "ores.refdata.api/domain/bundle_import_item.hpp"
#include "ores.refdata.api/domain/convention_family_registry.hpp"
#include "ores.refdata.core/service/conventions_document_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <iterator>
#include <limits>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::refdata::service {

using namespace ores::logging;

namespace {

/**
 * @brief How long a begin lets an uncommitted import wait.
 */
constexpr auto import_lifetime = std::chrono::hours(24);

/**
 * @brief Locks the import row for the rest of the commit's transaction.
 *
 * The id is written in its canonical form, so nothing but a uuid reaches the
 * statement text.
 */
void lock_import_row(const ores::database::context& ctx, const boost::uuids::uuid& id) {
    const auto& transaction = ctx.active_transaction();
    if (!transaction)
        throw std::logic_error("A commit must run inside its own transaction.");
    const auto sql = "select id from ores_refdata_bundle_imports_tbl where id = '" +
                     boost::uuids::to_string(id) +
                     "'::uuid and valid_to = ores_utility_infinity_timestamp_fn() for update";
    const auto result = (*transaction)->execute(sql);
    if (!result)
        throw std::runtime_error(result.error().what());
}

boost::uuids::uuid new_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

/**
 * @brief Whether the registry names the family a stage or commit carries.
 */
bool known_family(const std::string& family) {
    return std::any_of(std::begin(domain::convention_families),
                       std::end(domain::convention_families),
                       [&family](const auto& entry) { return entry.key == family; });
}

}

bundle_import_operations_service::bundle_import_operations_service(context ctx)
    : ctx_(std::move(ctx)) {}

messaging::begin_bundle_import_response
bundle_import_operations_service::begin(const messaging::begin_bundle_import_request& request) {
    messaging::begin_bundle_import_response response;
    if (request.id.is_nil()) {
        response.message = "A bundle import needs the caller's id.";
        return response;
    }
    if (request.state) {
        response.message = "The service states the import's state; a caller cannot set it.";
        return response;
    }
    if (!ctx_.party_id()) {
        response.message = "A bundle import needs a party in the session.";
        return response;
    }

    ores::database::repository::unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();
    const auto key = boost::uuids::to_string(request.id);
    auto existing = imports_.read_latest(ctx, key);
    if (!existing.empty()) {
        // A repeated begin names the import it already opened, so a retried
        // call opens no second session.
        uow.commit();
        response.success = true;
        response.import_row = std::move(existing.front());
        return response;
    }

    domain::bundle_import row;
    row.id = request.id;
    row.source = request.source;
    row.families_expected = request.families_expected;
    row.state = "staging";
    row.expires_at =
        request.expires_at.value_or(std::chrono::system_clock::now() + import_lifetime);
    ores::service::messaging::stamp(row, ctx);
    imports_.write(ctx, row);

    uow.commit();
    response.success = true;
    response.import_row = std::move(row);
    return response;
}

messaging::stage_bundle_family_response
bundle_import_operations_service::stage(const messaging::stage_bundle_family_request& request) {
    messaging::stage_bundle_family_response response;
    if (request.import_id.is_nil()) {
        response.message = "A staged family needs an import id.";
        return response;
    }
    if (request.family.empty()) {
        response.message = "A staged family needs a family.";
        return response;
    }
    if (!known_family(request.family)) {
        response.message =
            "The registry does not know the convention family '" + request.family + "'.";
        return response;
    }
    if (request.sequence < 0) {
        response.message = "A staged family's sequence cannot be negative.";
        return response;
    }
    if (!ctx_.party_id()) {
        response.message = "A staged family needs a party in the session.";
        return response;
    }

    ores::database::repository::unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();
    const auto key = boost::uuids::to_string(request.import_id);
    const auto import = imports_.read_latest(ctx, key);
    if (import.empty()) {
        response.message = "No bundle import named '" + key + "'.";
        return response;
    }
    if (import.front().state != "staging") {
        response.message =
            "Bundle import '" + key + "' is " + import.front().state + ", not staging.";
        return response;
    }

    const auto staged =
        items_.read_latest_by_import_id(ctx, key, 0, std::numeric_limits<std::uint32_t>::max());
    const auto duplicate = std::any_of(staged.begin(), staged.end(), [&request](const auto& item) {
        return item.family == request.family && item.sequence == request.sequence;
    });
    if (duplicate) {
        response.message = "Family '" + request.family + "' sequence " +
                           std::to_string(request.sequence) + " is already staged.";
        return response;
    }

    domain::bundle_import_item item;
    item.id = new_uuid();
    item.import_id = request.import_id;
    item.family = request.family;
    item.sequence = request.sequence;
    item.payload = request.payload;
    item.checksum = request.checksum;
    ores::service::messaging::stamp(item, ctx);
    items_.write(ctx, item);

    uow.commit();
    response.success = true;
    return response;
}

messaging::commit_bundle_import_response
bundle_import_operations_service::commit(const messaging::commit_bundle_import_request& request) {
    messaging::commit_bundle_import_response response;
    if (request.import_id.is_nil()) {
        response.message = "A commit needs an import id.";
        return response;
    }
    if (!ctx_.party_id()) {
        response.message = "A commit needs a party in the session.";
        return response;
    }

    try {
        ores::database::repository::unit_of_work uow(ctx_);
        const auto& ctx = uow.ctx();
        lock_import_row(ctx, request.import_id);

        const auto key = boost::uuids::to_string(request.import_id);
        const auto found = imports_.read_latest(ctx, key);
        if (found.empty()) {
            response.message = "No bundle import named '" + key + "'.";
            return response;
        }
        auto import = found.front();
        if (import.state != "staging") {
            response.message = "Bundle import '" + key + "' is " + import.state + ", not staging.";
            return response;
        }

        // Every stage of an import landed before its commit, so the read order
        // is the order the stages were committed in.
        const auto staged =
            items_.read_latest_by_import_id(ctx, key, 0, std::numeric_limits<std::uint32_t>::max());
        conventions_save_result applied;
        std::vector<messaging::bundle_import_family_outcome> outcomes;
        outcomes.reserve(staged.size());
        for (const auto& item : staged) {
            try {
                if (!apply_staged_family(ctx, item.family, item.payload, applied)) {
                    response.message =
                        "The registry does not know the convention family '" + item.family + "'.";
                    return response;
                }
            } catch (const std::exception& error) {
                // The unit of work rolls back, so the import stays staging.
                response.message = error.what();
                return response;
            }
            outcomes.push_back(messaging::bundle_import_family_outcome{
                .family = item.family, .sequence = item.sequence, .applied = true, .message = {}});
        }

        import.state = "applied";
        ores::service::messaging::stamp(
            import, ctx, ores::service::messaging::change_reasons::update);
        imports_.write(ctx, import);

        uow.commit();
        response.success = true;
        response.world_kept = std::move(applied.world_kept);
        response.families = std::move(outcomes);
    } catch (const std::exception& error) {
        // The unit of work's destructor rolls the transaction back, so a failed
        // commit leaves the import staging and the live tables untouched.
        response.message = error.what();
    }
    return response;
}

}
