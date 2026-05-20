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

#include <QHBoxLayout>
#include <QSizePolicy>
#include <QPointer>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.workspace.api/messaging/workspace_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

WorkspaceSelector::WorkspaceSelector(ClientManager* clientManager, QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)) {

    auto* layout = new QHBoxLayout(this);
    layout->setContentsMargins(4, 0, 4, 0);
    layout->setSpacing(4);

    label_ = new QLabel(tr("Workspace:"), this);
    label_->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    layout->addWidget(label_);

    combo_ = new QComboBox(this);
    combo_->setEditable(true);
    combo_->setInsertPolicy(QComboBox::NoInsert);
    combo_->setMinimumWidth(140);
    combo_->setMaximumWidth(220);
    combo_->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    combo_->setToolTip(tr("Select the workspace for this window"));

    auto* completer = new QCompleter(combo_->model(), this);
    completer->setFilterMode(Qt::MatchContains);
    completer->setCaseSensitivity(Qt::CaseInsensitive);
    combo_->setCompleter(completer);

    layout->addWidget(combo_);

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &WorkspaceSelector::onWorkspacesLoaded);
    connect(combo_, QOverload<int>::of(&QComboBox::activated),
            this, &WorkspaceSelector::onComboActivated);

    // Seed with Live only; async fetch replaces this on first show
    entries_.push_back({WorkspaceContext::live_workspace_id, QStringLiteral("Live")});
    populateCombo();

    refreshWorkspaces();
}

WorkspaceContext WorkspaceSelector::currentContext() const {
    return currentCtx_;
}

void WorkspaceSelector::setCurrentContext(const WorkspaceContext& ctx) {
    currentCtx_ = ctx;
    // Find the entry that matches and update the combo without triggering activated
    for (int i = 0; i < combo_->count(); ++i) {
        if (combo_->itemData(i).toString() == ctx.id) {
            const QSignalBlocker blocker(combo_);
            combo_->setCurrentIndex(i);
            return;
        }
    }
    // Not found yet (list still loading) — set the display text directly
    const QSignalBlocker blocker(combo_);
    combo_->setCurrentText(ctx.name);
}

void WorkspaceSelector::refreshWorkspaces() {
    if (fetching_ || !clientManager_ || !clientManager_->isConnected())
        return;
    fetching_ = true;

    QPointer<WorkspaceSelector> self = this;
    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        if (!self || !self->clientManager_) {
            return {false, {}, "Widget destroyed"};
        }
        workspace::messaging::list_workspaces_request req;
        req.limit = 500;
        auto result = self->clientManager_->process_authenticated_request(std::move(req));
        if (!result) {
            return {false, {}, QString::fromStdString(result.error())};
        }

        FetchResult fr{true, {}, {}};
        // Live sentinel always first
        fr.entries.push_back(
            {WorkspaceContext::live_workspace_id, QStringLiteral("Live")});
        for (const auto& ws : result->workspaces) {
            if (ws.status_code != "active") continue;
            WorkspaceEntry e;
            e.id   = QString::fromStdString(boost::uuids::to_string(ws.id));
            e.name = QString::fromStdString(ws.name);
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
        BOOST_LOG_SEV(lg(), warn) << "Failed to load workspaces: "
                                  << result.error.toStdString();
        return;
    }
    entries_ = std::move(result.entries);
    populateCombo();
    // Restore the current selection after repopulating
    setCurrentContext(currentCtx_);
}

void WorkspaceSelector::onComboActivated(int index) {
    if (index < 0 || index >= static_cast<int>(entries_.size()))
        return;
    const auto& e = entries_[static_cast<std::size_t>(index)];
    currentCtx_ = entryToContext(e);
    emit workspaceChanged(currentCtx_);
}

void WorkspaceSelector::populateCombo() {
    const QSignalBlocker blocker(combo_);
    combo_->clear();
    for (const auto& e : entries_) {
        combo_->addItem(e.name, e.id);
    }
}

WorkspaceContext WorkspaceSelector::entryToContext(const WorkspaceEntry& e) const {
    WorkspaceContext ctx;
    ctx.id   = e.id;
    ctx.name = e.name;
    ctx.resolution_order = {e.id};
    return ctx;
}

}
