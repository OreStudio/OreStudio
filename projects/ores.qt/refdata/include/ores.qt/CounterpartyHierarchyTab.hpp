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
#ifndef ORES_QT_COUNTERPARTY_HIERARCHY_TAB_HPP
#define ORES_QT_COUNTERPARTY_HIERARCHY_TAB_HPP

#include <QObject>
#include <boost/uuid/uuid.hpp>

class QTabWidget;
class QWidget;

namespace ores::qt {

class ClientManager;
class HierarchyTreeWidget;

/**
 * @brief Owns and drives the "Hierarchy" tab embedded in CounterpartyDetailDialog,
 * showing the subtree rooted at the displayed counterparty via parent_counterparty_id
 * (see the has_parent_id codegen flag). Not codegen'd: hand-written once
 * here and wired into the generated dialog via a small paste block.
 *
 * Uses the entity-agnostic HierarchyTreeWidget/HierarchyModelBuilder pair
 * (ores.qt.headless) -- this class's only job is fetching the tree over
 * NATS and handing the result to them.
 */
class CounterpartyHierarchyTab final : public QObject {
    Q_OBJECT

public:
    explicit CounterpartyHierarchyTab(QWidget* dialogParent);

    /** @brief Add the tab to the dialog's tab widget. Call once, from the constructor. */
    void attachTo(QTabWidget* tabWidget);

    /** @brief Re-fetch and redisplay the subtree rooted at counterpartyId. No-op if counterpartyId is nil. */
    void reload(const boost::uuids::uuid& counterpartyId, ClientManager* clientManager);

private:
    HierarchyTreeWidget* tree_{nullptr};
};

}

#endif
