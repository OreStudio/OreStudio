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
#include "ores.qt/PartyHierarchyTab.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HierarchyTreeWidget.hpp"
#include "ores.qt.headless/HierarchyModelBuilder.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include <QFutureWatcher>
#include <QPointer>
#include <QStandardItemModel>
#include <QTabWidget>
#include <QtConcurrent/QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

PartyHierarchyTab::PartyHierarchyTab(QWidget* dialogParent)
    : QObject(dialogParent)
    , tree_(new HierarchyTreeWidget(dialogParent)) {}

void PartyHierarchyTab::attachTo(QTabWidget* tabWidget) {
    tabWidget->addTab(tree_, "Hierarchy");
}

void PartyHierarchyTab::reload(const boost::uuids::uuid& partyId, ClientManager* clientManager) {
    if (partyId.is_nil() || !clientManager || !clientManager->isConnected())
        return;

    QPointer<PartyHierarchyTab> self = this;
    const auto rootIdStr = boost::uuids::to_string(partyId);

    auto task = [clientManager,
                 rootIdStr]() -> std::vector<ores::utility::domain::hierarchy_node> {
        refdata::messaging::get_party_hierarchy_request req;
        req.root_id = rootIdStr;
        req.from_root = false;
        auto result = clientManager->process_authenticated_request(std::move(req));
        if (!result || !result->success)
            return {};
        return result->roots;
    };

    auto* watcher = new QFutureWatcher<std::vector<ores::utility::domain::hierarchy_node>>(this);
    connect(watcher,
            &QFutureWatcher<std::vector<ores::utility::domain::hierarchy_node>>::finished,
            this,
            [self, watcher]() {
                auto roots = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                auto* model = HierarchyModelBuilder::build(roots);
                model->setParent(self->tree_);
                self->tree_->setModel(model);
                self->tree_->expandAll();
            });
    watcher->setFuture(QtConcurrent::run(task));
}

}
