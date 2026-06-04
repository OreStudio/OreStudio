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
#include "ores.qt/WorkspaceSelector.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.workspace.api/messaging/workspace_protocol.hpp"
#include <QHBoxLayout>
#include <QPointer>
#include <QSizePolicy>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

WorkspaceSelector::WorkspaceSelector(ClientManager* clientManager, QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , resolve_watcher_(new QFutureWatcher<ResolveResult>(this)) {

    auto* layout = new QHBoxLayout(this);
    layout->setContentsMargins(4, 2, 4, 2);
    layout->setSpacing(4);

    label_ = new QLabel(tr("Workspace:"), this);
    label_->setAlignment(Qt::AlignRight | Qt::AlignTop);
    label_->setContentsMargins(0, 4, 0, 0);
    layout->addWidget(label_);

    list_ = new QListWidget(this);
    list_->setSelectionMode(QAbstractItemView::SingleSelection);
    list_->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    list_->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    list_->setMaximumHeight(72);
    list_->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    list_->setToolTip(tr("Select the workspace for this window"));
    layout->addWidget(list_);

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &WorkspaceSelector::onWorkspacesLoaded);
    connect(resolve_watcher_,
            &QFutureWatcher<ResolveResult>::finished,
            this,
            &WorkspaceSelector::onResolutionLoaded);
    connect(
        list_, &QListWidget::currentRowChanged, this, &WorkspaceSelector::onListCurrentRowChanged);

    // Seed with Live only; async fetch replaces this on first show
    entries_.push_back({WorkspaceContext::live_workspace_id, QStringLiteral("Live"), {}});
    populateList();

    refreshWorkspaces();
}

WorkspaceContext WorkspaceSelector::currentContext() const {
    return currentCtx_;
}

void WorkspaceSelector::setCurrentContext(const WorkspaceContext& ctx) {
    currentCtx_ = ctx;
    const QSignalBlocker blocker(list_);
    for (int i = 0; i < list_->count(); ++i) {
        if (list_->item(i)->data(Qt::UserRole).toString() == ctx.id) {
            list_->setCurrentRow(i);
            return;
        }
    }
    // Entry not yet in list (still loading) — will be selected in populateList()
}

void WorkspaceSelector::refreshWorkspaces() {
    if (fetching_ || !clientManager_ || !clientManager_->isConnected())
        return;
    fetching_ = true;

    QPointer<WorkspaceSelector> self = this;
    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {false, {}, "Widget destroyed"};

        workspace::messaging::list_workspaces_request req;
        req.limit = 500;
        auto result = self->clientManager_->process_authenticated_request(std::move(req));
        if (!result)
            return {false, {}, QString::fromStdString(result.error())};

        // Build id→name map (all workspaces, not only active) for parent lookup
        QHash<QString, QString> id_to_name;
        id_to_name.insert(WorkspaceContext::live_workspace_id, QStringLiteral("Live"));
        for (const auto& ws : result->workspaces) {
            id_to_name.insert(QString::fromStdString(boost::uuids::to_string(ws.id)),
                              QString::fromStdString(ws.name));
        }

        FetchResult fr{true, {}, {}};
        fr.entries.push_back({WorkspaceContext::live_workspace_id, QStringLiteral("Live"), {}});
        for (const auto& ws : result->workspaces) {
            if (ws.status_code != "active")
                continue;
            WorkspaceEntry e;
            e.id = QString::fromStdString(boost::uuids::to_string(ws.id));
            e.name = QString::fromStdString(ws.name);
            if (ws.parent_workspace_id.has_value()) {
                const auto pid =
                    QString::fromStdString(boost::uuids::to_string(*ws.parent_workspace_id));
                e.parent_name = id_to_name.value(pid);
            }
            if (e.id != WorkspaceContext::live_workspace_id)
                fr.entries.push_back(std::move(e));
        }
        return fr;
    });
    watcher_->setFuture(future);
}

void WorkspaceSelector::onWorkspacesLoaded() {
    fetching_ = false;
    auto result = watcher_->result();
    if (!result.success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to load workspaces: " << result.error.toStdString();
        return;
    }
    entries_ = std::move(result.entries);
    populateList();
    setCurrentContext(currentCtx_);
}

void WorkspaceSelector::onListCurrentRowChanged(int row) {
    if (row < 0 || row >= static_cast<int>(entries_.size()))
        return;
    const auto& e = entries_[static_cast<std::size_t>(row)];
    if (e.id == currentCtx_.id)
        return; // no change
    if (e.id == WorkspaceContext::live_workspace_id) {
        currentCtx_ = entryToContext(e);
        emit workspaceChanged(currentCtx_);
    } else {
        resolveAndEmit(e);
    }
}

void WorkspaceSelector::resolveAndEmit(const WorkspaceEntry& e) {
    if (!clientManager_ || !clientManager_->isConnected()) {
        currentCtx_ = entryToContext(e);
        emit workspaceChanged(currentCtx_);
        return;
    }
    QPointer<WorkspaceSelector> self = this;
    const QString pending_id = e.id;
    const QString pending_name = e.name;
    QFuture<ResolveResult> future =
        QtConcurrent::run([self, pending_id, pending_name]() -> ResolveResult {
            if (!self || !self->clientManager_)
                return {false, {}, pending_id, pending_name};
            workspace::messaging::resolve_workspace_request req;
            req.workspace_id = pending_id.toStdString();
            auto result = self->clientManager_->process_authenticated_request(std::move(req));
            if (!result)
                return {false, {}, pending_id, pending_name};
            QVector<QString> order;
            for (const auto& wid : result->resolution_order)
                order.push_back(QString::fromStdString(wid));
            if (order.isEmpty())
                order.push_back(pending_id);
            return {true, std::move(order), pending_id, pending_name};
        });
    resolve_watcher_->setFuture(future);
}

void WorkspaceSelector::onResolutionLoaded() {
    auto result = resolve_watcher_->result();
    WorkspaceContext ctx;
    ctx.id = result.pending_id;
    ctx.name = result.pending_name;
    ctx.resolution_order =
        result.success ? result.resolution_order : QVector<QString>{result.pending_id};
    currentCtx_ = ctx;
    emit workspaceChanged(currentCtx_);
}

void WorkspaceSelector::populateList() {
    const QSignalBlocker blocker(list_);
    list_->clear();
    for (const auto& e : entries_) {
        QString text = e.name;
        if (!e.parent_name.isEmpty())
            text += QStringLiteral("  ·  ") + e.parent_name;
        auto* item = new QListWidgetItem(text);
        item->setData(Qt::UserRole, e.id);
        list_->addItem(item);
    }
}

WorkspaceContext WorkspaceSelector::entryToContext(const WorkspaceEntry& e) const {
    WorkspaceContext ctx;
    ctx.id = e.id;
    ctx.name = e.name;
    ctx.resolution_order = {e.id};
    return ctx;
}

}
