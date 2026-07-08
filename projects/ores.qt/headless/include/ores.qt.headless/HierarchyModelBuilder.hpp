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

#include "ores.qt.headless/export.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include <vector>

class QStandardItemModel;

namespace ores::qt {

/**
 * @brief Builds a QStandardItemModel representing a tree out of a forest of
 * ores::utility::domain::hierarchy_node values.
 *
 * The nesting (parent-linking, orphan/multi-root handling, duplicate-id
 * resolution) is already resolved by ores::utility::domain::build_tree();
 * this class only walks the given nesting and materialises the matching
 * QStandardItem tree.
 *
 * This class has no knowledge of party, counterparty, or any other domain
 * entity; it operates purely on hierarchy_node values.
 */
class ORES_QT_HEADLESS_API HierarchyModelBuilder final {
public:
    /**
     * @brief Build a tree model from a forest of nested hierarchy nodes.
     *
     * The returned model has a single column (the node name) and no
     * parent; ownership passes to the caller, which typically hands it
     * straight to HierarchyTreeWidget::setModel() or otherwise takes
     * ownership (e.g. by setting a QObject parent) to avoid leaking it.
     */
    static QStandardItemModel*
    build(const std::vector<ores::utility::domain::hierarchy_node>& roots);
};

}

#endif
