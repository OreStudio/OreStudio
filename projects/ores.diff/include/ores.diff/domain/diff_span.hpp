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
#ifndef ORES_DIFF_DOMAIN_DIFF_SPAN_HPP
#define ORES_DIFF_DOMAIN_DIFF_SPAN_HPP

#include <cstddef>

namespace ores::diff::domain {

/**
 * @brief One changed byte range within a diff_entry's old_value or
 * new_value, for intra-value highlighting.
 *
 * offset/length are byte offsets into the associated string, not
 * character or code-point counts; callers must not split a
 * multi-byte UTF-8 sequence when computing or consuming spans.
 */
struct diff_span final {
    std::size_t offset{};
    std::size_t length{};

    friend bool operator==(const diff_span&, const diff_span&) = default;
};

}

#endif
