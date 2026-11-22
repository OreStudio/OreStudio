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
#ifndef ORES_ORGMODE_CORE_SERVICE_KNOWLEDGE_INDEXER_HPP
#define ORES_ORGMODE_CORE_SERVICE_KNOWLEDGE_INDEXER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.orgmode.core/export.hpp"
#include <cstdint>

namespace ores::orgmode::service {

/**
 * @brief How much the last rebuild wrote.
 */
struct reindex_result final {
    std::uint64_t pages = 0;
    std::uint64_t sections = 0;
    std::uint64_t links = 0;
};

/**
 * @brief Rebuilds the knowledge index from the system tenant's knowledge pages.
 *
 * Reads every current page, derives the index with build_knowledge_index and
 * replaces the stored index with it in one call, so a reader sees the old
 * index or the new one.
 */
class ORES_ORGMODE_CORE_EXPORT knowledge_indexer {
public:
    explicit knowledge_indexer(const ores::database::context& ctx);

    reindex_result reindex();

private:
    ores::database::context ctx_;
};

}

#endif
