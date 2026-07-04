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
#ifndef ORES_QT_HIERARCHY_MODEL_BUILDER_HPP
#define ORES_QT_HIERARCHY_MODEL_BUILDER_HPP

#include "ores.qt/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

class QStandardItemModel;

namespace ores::qt {

/**
 * @brief A single node in a generic parent-child hierarchy.
 *
 * Entity-agnostic: carries only the id/parent-id/label triple needed to
 * build a tree. Callers are responsible for mapping their domain entity
 * (e.g. party, counterparty) into a list of these before handing it to
 * hierarchy_model_builder.
 */
struct hierarchy_node final {
    boost::uuids::uuid id{};
    std::optional<boost::uuids::uuid> parent_id;
    std::string label;
};

/**
 * @brief Builds a QStandardItemModel representing a tree out of a flat list
 * of hierarchy_node values.
 *
 * Root items are nodes with no parent_id, or nodes whose parent_id does not
 * refer to any other node in the input list (orphans). Orphans are attached
 * as additional roots rather than being dropped or causing a crash, since
 * badly-seeded data can violate the "one root per tenant" convention that
 * party/counterparty hierarchies otherwise follow.
 *
 * This class has no knowledge of party, counterparty, or any other domain
 * entity; it operates purely on hierarchy_node values.
 */
class ORES_QT_API HierarchyModelBuilder final {
public:
    /**
     * @brief Build a tree model from a flat list of hierarchy nodes.
     *
     * The returned model has a single column (the node label) and no
     * parent; ownership passes to the caller, which typically hands it
     * straight to HierarchyTreeWidget::setModel() or otherwise takes
     * ownership (e.g. by setting a QObject parent) to avoid leaking it.
     *
     * Nodes whose id appears more than once in the input are, as with any
     * malformed input, handled on a best-effort basis: the first occurrence
     * wins and later duplicates are treated as orphan roots.
     */
    static QStandardItemModel* build(const std::vector<hierarchy_node>& nodes);
};

}

#endif
