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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_REFDATA_CORE_SERVICE_CONVENTIONS_BUNDLE_HPP
#define ORES_REFDATA_CORE_SERVICE_CONVENTIONS_BUNDLE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.core/export.hpp"
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief One row of one family, as that family's wire payload carries it.
 *
 * The bundle does not know the row's type. It knows the family the row belongs
 * to, the order the caller gave it, and the row serialised as its own generated
 * type.
 */
struct conventions_bundle_row {
    std::string family;
    int sequence = 0;
    std::string payload;
};

/**
 * @brief The related families of one import, applied as one unit.
 *
 * A bundle is the largest atomic unit. The shared convention family and its
 * two part families are written first, because a part names its central row.
 * Every other family is applied in the order the bundle carries it, inside one
 * unit of work the caller owns.
 */
struct conventions_bundle {
    std::vector<conventions_bundle_row> rows;
};

/**
 * @brief What applying a bundle did not store, and why.
 */
struct conventions_apply_result {
    /**
     * World conventions the tenant already held. A bundle never changes world
     * data, so the tenant's row stands.
     */
    std::vector<std::string> world_kept;
};

/**
 * @brief Reads one family's live rows as the payloads the wire carries them in.
 *
 * The bytes are the bytes a stage message carries, so an in-process caller and
 * a remote caller see a family the same way. The generated dispatch gives each
 * family its own branch.
 *
 * @throws std::logic_error when the registry does not know the family.
 */
ORES_REFDATA_CORE_EXPORT std::vector<std::string>
read_family_payloads(const ores::database::context& ctx, const std::string& family);

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
