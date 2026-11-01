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
#ifndef ORES_REPORTING_CORE_SERVICE_CUT_AUTHORISATION_APPLIER_HPP
#define ORES_REPORTING_CORE_SERVICE_CUT_AUTHORISATION_APPLIER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.core/export.hpp"
#include <mutex>

namespace ores::reporting::service {

/**
 * @brief Stamps the decision of every decided cut-authorisation request onto
 * the cut it authorised.
 *
 * A reconciliation and not a reaction to one event: each run asks the database
 * which decided requests the cut has not been stamped with, across every
 * tenant, and writes each cut in its own tenant as the person who decided.
 * An approval stamps the cut's signer and instant and marks the request's
 * detail row applied; a refusal stamps the refusal and its reason, which is
 * terminal for that body. A row leaves the answer once applied, so a second run
 * writes nothing twice, a crash between the cut's write and the detail's mark
 * converges on the next run, and a decision taken while this service was down is
 * applied when it next runs.
 *
 * The facts a cut is stamped with are the decision's own — who decided and when
 * — and never the run's clock, so repeating a run cannot move them.
 *
 * The decision itself is not made here and is not checked here: four eyes, the
 * comment a refusal needs and the permission a decider holds are the inbox's
 * rules, enforced where the decision is written.
 */
class ORES_REPORTING_CORE_EXPORT cut_authorisation_applier {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.reporting.service.cut_authorisation_applier");
        return instance;
    }

public:
    explicit cut_authorisation_applier(ores::database::context ctx);

    /**
     * @brief Applies every decided request the cut is not yet stamped with;
     * answers how many were applied.
     *
     * A row that cannot be applied is logged and left for the next run, so one
     * tenant's problem does not hold up another's.
     */
    int apply();

private:
    ores::database::context ctx_;
    std::mutex mutex_;
};

}

#endif
