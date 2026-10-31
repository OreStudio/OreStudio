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
#ifndef ORES_REPORTING_CORE_SERVICE_CUT_RESOLUTION_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_CUT_RESOLUTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/service/cut_resolution_plan.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::service {

/**
 * @brief Answers the cut a report's selection resolves to, for a preview or a run.
 *
 * One operation serves both, because a preview computed by a second code path is
 * a preview of that path and not of the run: the rules are the
 * @ref resolve_cut_selection function and this service is what reads the store
 * for it.
 *
 * A kind whose instant the run supplies resolves to a body the run mints, and
 * the mint happens here: the take ordinal, the surrogate key and the kind's
 * retention are the store's to settle, and the retention copy is
 * @ref plan_take. A preview mints nothing, so a person sees the body the run
 * would fix without the run happening.
 */
class ORES_REPORTING_CORE_EXPORT cut_resolution_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.cut_resolution_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit cut_resolution_service(context ctx);

    /**
     * @brief Answers what a selection would resolve to right now, and writes nothing.
     *
     * The preview's sentence and the run's are one sentence, so a person reading
     * the report's page sees what a scheduled run would say.
     */
    cut_resolution preview(const cut_selection& selection, const cut_resolution_context& ctx);

    /**
     * @brief Resolves a selection for a run, minting the body where the kind fixes it.
     *
     * A run is refused when the body it must mint cannot be assembled, and the
     * refusal names the parts it could not fill: no partial body is minted.
     */
    cut_resolution resolve(const cut_selection& selection, const cut_resolution_context& ctx);

private:
    /**
     * @brief Writes the cut one mint describes, and answers the row it wrote.
     *
     * The take ordinal is the number of takes the occasion already holds, so a
     * second LIVE run in one second is its own body and its own label.
     */
    domain::cut mint(const cut_take_plan& plan, const domain::cut_kind& kind);

    cut_resolution run(const cut_selection& selection,
                       const cut_resolution_context& ctx,
                       bool preview_only);

    context ctx_;
};

}

#endif
