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
#include "ores.orgmode.core/service/knowledge_indexer.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.orgmode.core/repository/knowledge_page_repository.hpp"
#include "ores.orgmode.core/service/knowledge_index.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::orgmode::service {

using namespace ores::logging;

namespace {

inline std::string_view logger_name = "ores.orgmode.service.knowledge_indexer";
auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

}

knowledge_indexer::knowledge_indexer(const ores::database::context& ctx)
    : ctx_(ores::database::service::tenant_context::with_system_tenant(ctx)) {}

reindex_result knowledge_indexer::reindex() {
    repository::knowledge_page_repository repo;
    const auto pages = repo.read_latest(ctx_);
    const auto index = build_knowledge_index(pages);

    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx_,
        "SELECT pages, sections, links FROM ores_orgmode_replace_knowledge_index_fn($1::jsonb)",
        {ores::utility::codec::json::write(index)},
        lg(),
        "replace knowledge index");
    if (rows.size() != 1 || rows[0].size() != 3 || !rows[0][0] || !rows[0][1] || !rows[0][2])
        throw std::runtime_error("The knowledge index replace returned no counts.");

    const reindex_result result{.pages = std::stoull(*rows[0][0]),
                                .sections = std::stoull(*rows[0][1]),
                                .links = std::stoull(*rows[0][2])};
    ORES_LOG_SEV(lg(), info) << "Knowledge index rebuilt: pages=" << result.pages
                             << " sections=" << result.sections << " links=" << result.links;
    return result;
}

}
