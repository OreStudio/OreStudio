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
#ifndef ORES_INBOX_CORE_SERVICE_APPROVAL_APPLY_RETRY_HPP
#define ORES_INBOX_CORE_SERVICE_APPROVAL_APPLY_RETRY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/approval_apply_start.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include <chrono>
#include <string>
#include <vector>

namespace ores::inbox::service {

/**
 * @brief The engine, as the scheduled retry of an apply needs it.
 *
 * The service implements it over the wire with its own token. A test
 * implements it with a record of what it was asked, so the retry's decisions
 * are tested without an engine.
 */
class apply_engine {
public:
    virtual ~apply_engine() = default;

    /**
     * @brief Publishes the start of a run. A start that names a run the engine
     * holds changes nothing.
     */
    virtual void start(const ores::workflow::messaging::start_workflow_message& start) = 0;

    /**
     * @brief Asks the engine to resume a stopped run from the step that failed.
     *
     * The engine refuses a run that has not stopped, and says so in the answer.
     */
    virtual ores::workflow::messaging::retry_workflow_instance_response
    retry(const std::string& workflow_instance_id) = 0;
};

/**
 * @brief What one pass of the retry did, as request ids.
 */
struct apply_retry_outcome {
    std::vector<std::string> resumed;
    std::vector<std::string> gave_up;
};

/**
 * @brief The reason a request carries when its apply ran out of time.
 */
inline constexpr const char* could_not_apply = "could not apply";

/**
 * @brief Resumes the applies that stopped and ends the ones that ran out of
 * time.
 *
 * An approved request whose kind names an apply workflow stays approved until
 * the workflow applies it. For each such request this either ends it, when the
 * kind's window has ended, or starts its run again and asks the engine to resume
 * it. Both engine calls are safe to repeat: a start that names a run the engine
 * holds changes nothing, and a resume of a run that has not stopped is refused.
 *
 * A run that stopped on a refusal is never retried, because the refusal moves
 * its request to apply_failed and the request is no longer approved.
 */
class ORES_INBOX_CORE_EXPORT approval_apply_retry {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.inbox.service.approval_apply_retry");
        return instance;
    }

public:
    approval_apply_retry(ores::database::context ctx, apply_engine& engine);

    /**
     * @brief Runs one pass over every tenant's stalled applies.
     *
     * A request whose window ended before @p as_of is ended rather than
     * resumed. The moment is a parameter so a test can ask about a later one.
     *
     * One request failing does not stop the others: the failure is logged and
     * the next pass tries it again.
     */
    apply_retry_outcome run(std::chrono::system_clock::time_point as_of =
                                std::chrono::system_clock::now());

private:
    void give_up(approval_lifecycle& lifecycle,
                 const ores::database::context& ctx,
                 const domain::approval_kind& kind,
                 const domain::approval_request& request);

    ores::database::context ctx_;
    apply_engine& engine_;
};

}

#endif
