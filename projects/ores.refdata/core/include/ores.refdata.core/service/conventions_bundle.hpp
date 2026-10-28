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
#ifndef ORES_REFDATA_CORE_SERVICE_CONVENTIONS_BUNDLE_HPP
#define ORES_REFDATA_CORE_SERVICE_CONVENTIONS_BUNDLE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/messaging/conventions_bundle.hpp"
#include "ores.refdata.core/export.hpp"
#include <string>

namespace ores::refdata::service {

/**
 * @brief The bundle's rows and result are the contract's types.
 *
 * A caller that reads an ORE conventions document and the service that writes
 * the families both name them, so they live in the api and this service only
 * reads and applies them.
 */
using messaging::conventions_apply_result;
using messaging::conventions_bundle;
using messaging::conventions_bundle_row;

/**
 * @brief Applies one family's payload, in the way that family writes.
 *
 * The payload is the family's own generated value type, serialised as JSON.
 * The branch for the family decodes it, stamps the session's party on the row,
 * and writes it: a party family replaces the party's rows, a by-id family
 * replaces the rows the payload names, and a world family adds only the rows
 * the tenant lacks.
 *
 * @return false when the registry does not know the family.
 * @throws std::runtime_error when the payload does not decode into the family's
 * type, so a commit rolls back rather than applying a partial family.
 */
ORES_REFDATA_CORE_EXPORT bool apply_staged_family(const ores::database::context& ctx,
                                                  const std::string& family,
                                                  const std::string& payload,
                                                  conventions_apply_result& result);

/**
 * @brief Applies the related families of one import.
 *
 * One writer, so a second caller cannot skip a step the first performs. The
 * service does not open a unit of work: the caller's transaction covers the
 * whole bundle, and a family that fails rolls back every other family in it.
 */
class ORES_REFDATA_CORE_EXPORT conventions_bundle_service {
public:
    using context = ores::database::context;

    explicit conventions_bundle_service(context ctx);

    /**
     * @brief Applies every row the bundle carries.
     *
     * The shared convention row and its two part rows are resolved and written
     * together first, because a part names its central row. Every other family
     * is applied in the order the bundle carries it, through the generated
     * apply.
     *
     * @throws std::runtime_error when the registry does not know a family, or
     * when a payload does not decode into its family's type.
     */
    conventions_apply_result apply(const conventions_bundle& bundle);

    /**
     * @brief Every convention the session sees, one payload per row.
     *
     * The party's own instrument conventions and the tenant's world
     * conventions, read one family at a time. The shared convention family is
     * narrowed to the kind this bundle covers, because that one table carries
     * every kind.
     */
    conventions_bundle read();

private:
    context ctx_;
};

}

#endif
