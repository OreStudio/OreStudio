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
#include "ores.inbox.core/service/approval_apply_retry.hpp"
#include <exception>
#include <utility>

namespace ores::inbox::service {

using namespace ores::logging;

approval_apply_retry::approval_apply_retry(ores::database::context ctx, apply_engine& engine)
    : ctx_(std::move(ctx))
    , engine_(engine) {}

apply_retry_outcome approval_apply_retry::run(std::chrono::system_clock::time_point as_of) {
    apply_retry_outcome outcome;

    for (const auto& stalled : approval_lifecycle(ctx_).stalled_applies(as_of)) {
        try {
            // The request is read and changed in its own tenant, as the
            // service's account, because the sweep itself belongs to no tenant.
            const auto tenant =
                ores::database::service::tenant_context::with_tenant(ctx_, stalled.tenant_id);
            const auto ctx = tenant.with_tenant(tenant.tenant_id(), tenant.service_account());
            approval_lifecycle lifecycle(ctx);

            const auto request = lifecycle.request(stalled.request_id);
            const auto kind = lifecycle.kind(stalled.kind_code);
            if (!request || !kind || request->state_code != "approved") {
                BOOST_LOG_SEV(lg(), warn)
                    << "Request " << stalled.request_id << " could not be read as an approved "
                    << "request of kind " << stalled.kind_code << ", so its apply was left alone.";
                continue;
            }

            const auto start = apply_start_of(*kind, *request, "");
            if (!start)
                continue;

            if (stalled.over_window) {
                // Only a run that stopped, or that never existed, is given up.
                // A run still working may yet apply the request.
                const auto status = engine_.run_status(start->instance_id);
                if (status.empty() || status == "failed") {
                    give_up(lifecycle, ctx, *kind, *request);
                    outcome.gave_up.push_back(stalled.request_id);
                } else {
                    BOOST_LOG_SEV(lg(), debug) << "Request " << stalled.request_id
                                               << " is past its window, but its run is " << status
                                               << ", so it was left alone.";
                }
                continue;
            }

            // The start repairs a request whose start was lost. When the run
            // exists the engine leaves it alone, so the resume below is what
            // moves a run that stopped.
            engine_.start(*start);
            const auto resumed = engine_.retry(start->instance_id);
            if (resumed.success) {
                BOOST_LOG_SEV(lg(), info)
                    << "Request " << stalled.request_id
                    << " had a stopped apply; the engine resumed step " << resumed.step_name << ".";
                outcome.resumed.push_back(stalled.request_id);
            } else {
                BOOST_LOG_SEV(lg(), debug)
                    << "Request " << stalled.request_id << " was not resumed: " << resumed.message;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Retrying the apply of request " << stalled.request_id
                                       << " failed: " << e.what();
        }
    }
    return outcome;
}

void approval_apply_retry::give_up(approval_lifecycle& lifecycle,
                                   const ores::database::context& ctx,
                                   const domain::approval_kind& kind,
                                   const domain::approval_request& request) {
    const auto result = lifecycle.fail_apply(boost::uuids::to_string(request.id), could_not_apply);
    if (result.outcome != "ok")
        throw std::runtime_error("the request did not move to apply_failed: " + result.message);

    BOOST_LOG_SEV(lg(), warn) << "Request " << boost::uuids::to_string(request.id) << " ("
                              << kind.code << ") could not be applied within its window.";

    // The announcer logs a failure to tell and never throws.
    const auto after = lifecycle.request(boost::uuids::to_string(request.id)).value_or(request);
    approval_announcer(ctx).tell_asker(kind, after, "apply_failed", could_not_apply);
}

}
