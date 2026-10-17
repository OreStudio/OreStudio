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
#ifndef ORES_REFDATA_CORE_SERVICE_TENOR_RESOLUTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENOR_RESOLUTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ores.refdata.core/export.hpp"

namespace ores::refdata::service {

/**
 * @brief Resolves a tenor to a date from the persisted rows, through the
 * pure engine.
 *
 * The mapping is all this service does: it reads the tenor, the convention
 * and the resolution row, materialises the named calendar's business days,
 * hands the lot to @c ores::analytics::quant::service::tenor_resolution_engine
 * and reports what came back. No date arithmetic lives here, and no second
 * implementation of the rules is allowed to grow here -- the engine is the
 * one place the conventions are stated.
 *
 * Every refusal is an answer, not an exception: a missing row, an unknown
 * code, a calendar nobody materialised and a rule the engine declines are all
 * a @c result with a non-ok outcome, the step that refused in its @c code,
 * the dates counted so far on the response, and the row at fault named in its
 * @c fields. The caller that must know which of those happened reads it from
 * the payload.
 */
class ORES_REFDATA_CORE_EXPORT tenor_resolution_service final {
public:
    using context = ores::database::context;

    /**
     * @brief Constructs the service with a database context.
     *
     * @param ctx The database context, carrying the tenant every read is
     * scoped to.
     */
    explicit tenor_resolution_service(context ctx);

    /**
     * @brief Resolves the request's tenor under its convention.
     *
     * @param request The tenor, convention and calendar to resolve, the
     * horizon to count from, and the spot date when the convention's anchor
     * needs one.
     * @return The response, filled in every case: the dates and roll rule
     * when the resolution stands, or a @c result saying which step refused.
     */
    [[nodiscard]] messaging::resolve_tenor_response
    resolve(const messaging::resolve_tenor_request& request) const;

private:
    context ctx_;
};

} // namespace ores::refdata::service

#endif
