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
#include "ores.reporting.core/service/cut_authorisation_applier.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.core/repository/cut_approval_detail_repository.hpp"
#include "ores.reporting.core/repository/cut_repository.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>

namespace ores::reporting::service {

using namespace ores::logging;
namespace db = ores::database::repository;
using ores::platform::time::datetime;

namespace {

/// The decision that signs a body, and the one that ends it for good.
constexpr std::string_view approve_decision = "approve";
constexpr std::string_view refuse_decision = "refuse";

/**
 * @brief One decided request the cut has not been stamped with.
 *
 * The instant is kept at the precision the store holds, so a stamp written from
 * it compares equal to the same decision read back on a later run.
 */
struct unapplied_authorisation {
    std::string tenant_id;
    std::string request_id;
    std::string detail_id;
    std::string cut_id;
    std::string approval_kind_code;
    std::string decision_code;
    std::string decided_by;
    std::chrono::system_clock::time_point decided_at;
    std::string comment;
};

/** The instant a decision holds, at the precision the store keeps. */
std::chrono::system_clock::time_point at_store_precision(
    const std::chrono::system_clock::time_point& t) {
    return std::chrono::floor<std::chrono::microseconds>(t);
}

/**
 * @brief Whether the cut already carries the facts this decision states.
 *
 * A stamp that is already there is not written again: the row keeps the version
 * it has, and the run only marks the request applied. That is what makes a
 * repeated run leave the cut untouched rather than churning a version of it.
 */
bool already_stamped(const domain::cut& cut, const unapplied_authorisation& decision) {
    if (decision.decision_code == approve_decision)
        return cut.approved_by == decision.decided_by && cut.approved_at &&
               at_store_precision(*cut.approved_at) == decision.decided_at &&
               !cut.refused_at && cut.refusal_comment.empty();
    return cut.refused_at && at_store_precision(*cut.refused_at) == decision.decided_at &&
           cut.refusal_comment == decision.comment && cut.approved_by.empty() && !cut.approved_at;
}

/**
 * @brief The cut as this decision leaves it.
 *
 * An approval clears a refusal, and a refusal clears an approval, so the two
 * facts a cut can hold are never both stated; the store refuses a row that
 * states both. A refusal copies the reason as the decider wrote it, because the
 * cut's page shows that sentence and the remedy is a new take.
 */
void stamp(domain::cut& cut, const unapplied_authorisation& decision, const std::string& actor) {
    if (decision.decision_code == approve_decision) {
        cut.approved_by = decision.decided_by;
        cut.approved_at = decision.decided_at;
        cut.refused_at.reset();
        cut.refusal_comment.clear();
    } else {
        cut.refused_at = decision.decided_at;
        cut.refusal_comment = decision.comment;
        cut.approved_by = std::string{};
        cut.approved_at.reset();
    }
    cut.modified_by = actor;
    cut.change_reason_code = "system.update";
    cut.change_commentary = "The decision on request " + decision.request_id + " applied";
}

} // namespace

cut_authorisation_applier::cut_authorisation_applier(ores::database::context ctx)
    : ctx_(std::move(ctx)) {}

int cut_authorisation_applier::apply() {
    std::lock_guard lock(mutex_);

    const auto rows = db::execute_parameterized_multi_column_query(
        ctx_,
        "select tenant_id::text, request_id::text, detail_id::text, cut_id::text,"
        " approval_kind_code, decision_code, decided_by, decided_at::text, comment"
        " from ores_reporting_unapplied_cut_authorisations_fn()",
        {},
        lg(),
        "Reading decided cut authorisations not yet applied");

    int applied = 0;
    for (const auto& row : rows) {
        try {
            unapplied_authorisation decision;
            decision.tenant_id = row[0].value_or("");
            decision.request_id = row[1].value_or("");
            decision.detail_id = row[2].value_or("");
            decision.cut_id = row[3].value_or("");
            decision.approval_kind_code = row[4].value_or("");
            decision.decision_code = row[5].value_or("");
            decision.decided_by = row[6].value_or("");
            decision.decided_at = at_store_precision(datetime::from_db_string(row[7].value_or("")));
            decision.comment = row[8].value_or("");
            // The read answers both decisions a cut can take; anything else a
            // kind of request may reach is not a decision on the body.
            if (decision.decision_code != approve_decision &&
                decision.decision_code != refuse_decision)
                continue;

            const auto tenant = ores::utility::uuid::tenant_id::from_string(decision.tenant_id);
            if (!tenant)
                throw std::runtime_error("bad tenant " + decision.tenant_id);

            // The cut is written in its own tenant and as the person who
            // decided, so the audit tail of the row names them and not the
            // service that happened to run the reconciliation.
            const auto ctx = ctx_.with_tenant(*tenant, decision.decided_by);
            const auto applier = ctx.service_account().empty() ? decision.decided_by
                                                               : ctx.service_account();

            db::unit_of_work uow(ctx);
            const auto& uow_ctx = uow.ctx();

            repository::cut_repository cuts;
            const auto held = cuts.read_latest(uow_ctx, decision.cut_id);
            if (held.empty())
                throw std::runtime_error("no cut " + decision.cut_id +
                                         " for request " + decision.request_id);

            auto cut = held.front();
            if (!already_stamped(cut, decision)) {
                stamp(cut, decision, decision.decided_by);
                cut.performed_by = applier;
                cuts.write(uow_ctx, cut);
            }

            // The detail row is the marker that the decision reached the cut,
            // and it is written in the same act: a request marked applied whose
            // cut was never stamped would be a decision nothing reads again.
            repository::cut_approval_detail_repository details;
            const auto details_held = details.read_latest(uow_ctx, decision.detail_id);
            if (details_held.empty())
                throw std::runtime_error("no detail " + decision.detail_id +
                                         " for request " + decision.request_id);
            auto detail = details_held.front();
            detail.applied_by = applier;
            detail.applied_at = decision.decided_at;
            detail.modified_by = decision.decided_by;
            detail.performed_by = applier;
            detail.change_reason_code = "system.update";
            detail.change_commentary = decision.decision_code == approve_decision
                                           ? "Approved: the cut's authorisation is stamped"
                                           : "Refused: the cut's refusal is stamped";
            details.write(uow_ctx, detail);

            uow.commit();
            ++applied;
            BOOST_LOG_SEV(lg(), info) << "Applied the " << decision.decision_code << " of request "
                                      << decision.request_id << " to cut " << decision.cut_id;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Applying request " << row[1].value_or("") << " to cut " << row[3].value_or("")
                << " failed; the next run tries again: " << e.what();
        }
    }
    return applied;
}

}
