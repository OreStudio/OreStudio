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
#include "ores.qt/OrgDocViewerWindow.hpp"
#include "ores.orgmode/indexing/resolver.hpp"
#include "ores.orgmode/parser/parser.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/OrgDocRenderer.hpp"
#include <QAction>
#include <QDir>
#include <QFileInfo>
#include <QTextBrowser>
#include <QToolBar>
#include <QUrl>
#include <QVBoxLayout>
#include <memory>

namespace ores::qt {

namespace {

QString find_repo_file(const QString& reference_path, const QString& filename) {
    QDir dir(QFileInfo(reference_path).absolutePath());
    for (int i = 0; i < 10; ++i) {
        const QString candidate = dir.filePath(filename);
        if (QFileInfo::exists(candidate))
            return candidate;
        if (!dir.cdUp())
            break;
    }
    return {};
}

}

OrgDocViewerWindow::OrgDocViewerWindow(const QString& referencePath, QWidget* parent)
    : QWidget(parent)
    , referencePath_(referencePath) {
    // Deliberately not a top-level window (no WA_DeleteOnClose, no
    // Qt::Window flag) — the caller hosts this as the content of a
    // DetachableMdiSubWindow, the same as every other detail window in
    // the app, so it's plain embedded content here.
    auto* layout = new QVBoxLayout(this);
    toolBar_ = new QToolBar(this);
    backAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowLeft, IconUtils::DefaultIconColor), tr("Back"));
    forwardAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowRight, IconUtils::DefaultIconColor),
        tr("Forward"));
    layout->addWidget(toolBar_);

    browser_ = new QTextBrowser(this);
    browser_->setOpenLinks(false);
    layout->addWidget(browser_);

    connect(browser_, &QTextBrowser::anchorClicked, this, [this](const QUrl& url) {
        const QString target = url.toString();
        if (target.startsWith("id:"))
            navigateToId(target.mid(3));
    });
    connect(backAction_, &QAction::triggered, this, [this]() {
        if (historyIndex_ > 0) {
            --historyIndex_;
            showCurrent();
        }
    });
    connect(forwardAction_, &QAction::triggered, this, [this]() {
        if (historyIndex_ + 1 < static_cast<int>(history_.size())) {
            ++historyIndex_;
            showCurrent();
        }
    });
    updateNavActions();

    resize(600, 500);
}

void OrgDocViewerWindow::navigateToId(const QString& id) {
    if (historyIndex_ + 1 < static_cast<int>(history_.size()))
        history_.resize(static_cast<std::size_t>(historyIndex_ + 1));
    history_.push_back(id);
    ++historyIndex_;
    showCurrent();
}

void OrgDocViewerWindow::showCurrent() {
    updateNavActions();
    if (historyIndex_ < 0 || historyIndex_ >= static_cast<int>(history_.size()))
        return;
    const QString id = history_[static_cast<std::size_t>(historyIndex_)];

    const QString db_path = find_repo_file(referencePath_, ".org-roam.db");
    if (db_path.isEmpty()) {
        browser_->setPlainText(
            tr("(Could not resolve — is the repo's org-roam index built? Run 'compass index'.)"));
        return;
    }
    std::unique_ptr<orgmode::indexing::resolver> resolver;
    try {
        resolver = std::make_unique<orgmode::indexing::resolver>(db_path.toStdString());
    } catch (const std::exception&) {
        browser_->setPlainText(
            tr("(Could not resolve — is the repo's org-roam index built? Run 'compass index'.)"));
        return;
    }

    const auto target = resolver->resolve(id.toStdString());
    if (!target) {
        browser_->setPlainText(
            tr("(Dangling link: id:%1 not found in the org-roam index.)").arg(id));
        return;
    }
    setWindowTitle(QString::fromStdString(target->title));
    try {
        const auto doc = orgmode::parser::parse_file(target->path);
        browser_->setHtml(render_org_doc_to_html(doc));
    } catch (const std::exception& e) {
        browser_->setPlainText(tr("(Could not parse '%1': %2)")
                                    .arg(QString::fromStdString(target->path),
                                         QString::fromStdString(e.what())));
    }
}

void OrgDocViewerWindow::updateNavActions() {
    backAction_->setEnabled(historyIndex_ > 0);
    forwardAction_->setEnabled(historyIndex_ + 1 < static_cast<int>(history_.size()));
}

}
