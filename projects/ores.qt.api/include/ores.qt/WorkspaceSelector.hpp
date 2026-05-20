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
#ifndef ORES_QT_WORKSPACE_SELECTOR_HPP
#define ORES_QT_WORKSPACE_SELECTOR_HPP

#include <vector>
#include <QWidget>
#include <QComboBox>
#include <QCompleter>
#include <QLabel>
#include <QFutureWatcher>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/WorkspaceContext.hpp"
#include "ores.qt/export.hpp"

namespace ores::qt {

class ClientManager;

/**
 * @brief Compact searchable workspace selector for MDI window toolbars.
 *
 * Shows the current workspace and lets the user switch to any active workspace.
 * Fetches the workspace list from the server asynchronously on first show.
 * Emits workspaceChanged when the selection changes.
 *
 * Usage in a window's setupToolbar():
 * @code
 *   auto* spacer = new QWidget();
 *   spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
 *   toolbar_->addWidget(spacer);
 *   workspaceSelector_ = new WorkspaceSelector(clientManager_, this);
 *   workspaceSelector_->setCurrentContext(windowWorkspaceContext_);
 *   toolbar_->addWidget(workspaceSelector_);
 * @endcode
 */
class ORES_QT_API WorkspaceSelector : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.workspace_selector";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit WorkspaceSelector(ClientManager* clientManager,
                                QWidget* parent = nullptr);
    ~WorkspaceSelector() override = default;

    WorkspaceContext currentContext() const;
    void setCurrentContext(const WorkspaceContext& ctx);

    /**
     * @brief Fetch (or re-fetch) the workspace list from the server.
     */
    void refreshWorkspaces();

signals:
    void workspaceChanged(const WorkspaceContext& ctx);

private slots:
    void onWorkspacesLoaded();
    void onComboActivated(int index);
    void onResolutionLoaded();

private:
    struct WorkspaceEntry {
        QString id;
        QString name;
    };

    struct FetchResult {
        bool success;
        std::vector<WorkspaceEntry> entries;
        QString error;
    };

    struct ResolveResult {
        bool success;
        QVector<QString> resolution_order;
        QString pending_id;
        QString pending_name;
    };

    void populateCombo();
    WorkspaceContext entryToContext(const WorkspaceEntry& e) const;
    void resolveAndEmit(const WorkspaceEntry& e);

    ClientManager* clientManager_;
    WorkspaceContext currentCtx_;
    std::vector<WorkspaceEntry> entries_;

    QLabel* label_;
    QComboBox* combo_;
    QFutureWatcher<FetchResult>* watcher_;
    QFutureWatcher<ResolveResult>* resolve_watcher_;
    bool fetching_{false};
};

}

#endif
