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
#ifndef ORES_DIFF_DOMAIN_DIFF_RESULT_HPP
#define ORES_DIFF_DOMAIN_DIFF_RESULT_HPP

#include <vector>
#include "ores.diff/domain/diff_entry.hpp"

namespace ores::diff::domain {

/**
 * @brief The ordered field-level differences between two versions.
 *
 * Entry order follows the current version's field order (the mapper
 * order), with fields removed from the current version appended last
 * in the previous version's order. Embeddable in protocol messages
 * via rfl serialisation.
 */
struct diff_result final {
    /**
     * @brief One entry per changed, added or removed field; empty
     * when the versions are identical.
     */
    std::vector<diff_entry> entries;

    friend bool operator==(const diff_result&, const diff_result&) = default;
};

}

#endif
