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
#ifndef ORES_WORKFLOW_CORE_REPOSITORY_STEP_CLAIM_REPOSITORY_HPP
#define ORES_WORKFLOW_CORE_REPOSITORY_STEP_CLAIM_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.workflow.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::workflow::repository {

/**
 * @brief The claim that makes exactly one delivery do a step's work.
 *
 * A step command is delivered at least once, so two deliveries for one step can
 * arrive at once and both find the step still in_progress. This table is what
 * decides between them: one takes a lease and works, the other is told someone
 * else holds it and comes back later.
 *
 * The claim belongs to the installation. Only a session of the system tenant
 * reads or writes it, and the policy decides that, so every method here runs
 * with the service's own context and states no tenant predicate. The step's own
 * tenant is carried alongside for diagnostics only.
 */
class ORES_WORKFLOW_CORE_EXPORT step_claim_repository {
private:
    inline static std::string_view logger_name = "ores.workflow.repository.step_claim_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Take the claim on a step, or find that someone else holds it.
     *
     * One statement inserts the claim, or takes over the row when the lease on
     * it has ended, so two callers arriving together cannot both succeed: the
     * store serialises them and the loser's conditional update matches no row.
     *
     * A live lease is a normal answer rather than a failure. The step's work is
     * running somewhere else, and the delivery that is told so returns to the
     * queue until the lease ends.
     *
     * @param owner Who holds it, so an operator can see which process is working
     *              the step. The claiming service names itself, because it is
     *              the one running the work.
     * @param lease How long the claim is held before another caller may take it.
     *              It has to outlast the step's slowest healthy run, and it is
     *              the only thing that releases a claim whose holder died.
     *
     * @return The attempt count, counting this claim, when this caller now owns
     *         the claim; nothing when another caller holds a live one.
     */
    std::optional<int> claim(context ctx,
                             const boost::uuids::uuid& step_id,
                             const boost::uuids::uuid& work_tenant_id,
                             const std::string& owner,
                             std::chrono::seconds lease);

    /**
     * @brief Take the claim on a step away.
     *
     * Called by the engine when it writes the step's state away from
     * in_progress. The step's own state is the authority on whether a replay
     * may run, and the claim governs only the window while the work is in
     * flight; the engine is what closes that window, so it releases the claim
     * whatever the holder named itself. The owner is therefore not part of the
     * predicate: the write is the engine's act, not the holder's.
     *
     * @return True when a claim was there to remove.
     */
    bool release(context ctx, const boost::uuids::uuid& step_id);

    /**
     * @brief The owner of the claim on a step, when one holds it.
     *
     * Reads only, for diagnostics: a step whose deliveries keep coming back
     * busy names the process that is keeping them out. The engine also reads
     * it to tell a step a worker has taken from one still waiting in a queue.
     */
    std::optional<std::string> holder(context ctx, const boost::uuids::uuid& step_id);

    /**
     * @brief When the current claim on a step was taken, when one holds it.
     *
     * The moment a worker took the step, which is when the work could first go
     * silent. A command that waited in a queue before that was waiting on the
     * service, not on the step.
     */
    std::optional<std::chrono::system_clock::time_point>
    claimed_at(context ctx, const boost::uuids::uuid& step_id);
};

}

#endif
