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
#ifndef ORES_REFDATA_API_MESSAGING_CONVENTIONS_BUNDLE_HPP
#define ORES_REFDATA_API_MESSAGING_CONVENTIONS_BUNDLE_HPP

#include <string>
#include <vector>

/**
 * @file conventions_bundle.hpp
 * @brief The rows of one conventions import, as the stage and commit messages
 * carry them.
 *
 * The bundle is the contract between a caller that reads an ORE conventions
 * document and the service that writes the families: the caller decides which
 * family each row is, and the service applies them as one unit. It holds no
 * database and no codec, so both sides can name it.
 */
namespace ores::refdata::messaging {

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

}

#endif
