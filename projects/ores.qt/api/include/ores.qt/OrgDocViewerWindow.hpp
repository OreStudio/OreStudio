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
#ifndef ORES_QT_ORG_DOC_VIEWER_WINDOW_HPP
#define ORES_QT_ORG_DOC_VIEWER_WINDOW_HPP

#include "ores.qt/export.hpp"
#include <QString>
#include <QWidget>
#include <vector>

class QAction;
class QTextBrowser;
class QToolBar;

namespace ores::qt {

/**
 * @brief A small, independent, non-modal window for browsing org docs
 * by following =[[id:...][...]]= links, with its own Back/Forward
 * history — spawned when a link inside a Story/Task tab (or a step's
 * body) is clicked, so following a trail of links doesn't navigate
 * away from whatever the tester was already looking at.
 *
 * Plain embedded content, not a top-level window: the caller hosts it
 * as the widget of a =DetachableMdiSubWindow= (added to the app's own
 * MDI area), the same convention every other detail window in the app
 * follows — never a detached window, which several window managers
 * pin above the whole application regardless of modality.
 *
 * A fresh read-only handle onto the org-roam index (found by walking
 * up from @p referencePath) is opened on every navigation rather than
 * held as a member — cheap enough for a manual-testing tool that
 * resolves a handful of links per session, and one less lifetime to
 * manage.
 */
class ORES_QT_API OrgDocViewerWindow final : public QWidget {
    Q_OBJECT

public:
    /**
     * @param referencePath Any path under the repo (e.g. the scenario
     * doc's own path) — used only to locate =.org-roam.db= by walking
     * up the directory tree.
     */
    explicit OrgDocViewerWindow(const QString& referencePath, QWidget* parent = nullptr);

public slots:
    /**
     * @brief Navigate to @p id, pushing it onto the history (discarding
     * any forward history past the current position, same as a normal
     * browser).
     */
    void navigateToId(const QString& id);

private:
    void showCurrent();
    void updateNavActions();

    QString referencePath_;
    QToolBar* toolBar_;
    QAction* backAction_;
    QAction* forwardAction_;
    QTextBrowser* browser_;

    std::vector<QString> history_;
    int historyIndex_ = -1;
};

}

#endif
