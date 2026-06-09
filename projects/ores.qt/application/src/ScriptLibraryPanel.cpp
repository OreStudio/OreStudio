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
#include "ores.qt/ScriptLibraryPanel.hpp"
#include "ores.qt/IconUtils.hpp"
#include <QCoreApplication>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QLabel>
#include <QMenu>
#include <QMessageBox>
#include <QStandardPaths>
#include <QTextStream>
#include <QVBoxLayout>

namespace ores::qt {

namespace {

constexpr int role_path = Qt::UserRole;
constexpr int role_library = Qt::UserRole + 1;

/**
 * @brief Read a script's human description from the leading comment
 * block the generator writes (the recipe title and description), so the
 * tree can show what each .ores does as a tooltip. The generated-file
 * banner lines are skipped. Empty when the file has no such header.
 */
QString description_for(const QString& ores_path) {
    QFile f(ores_path);
    if (!f.open(QIODevice::ReadOnly | QIODevice::Text))
        return {};

    QStringList lines;
    QTextStream in(&f);
    while (!in.atEnd()) {
        const QString line = in.readLine();
        if (!line.startsWith("#")) // header is the leading comment block
            break;
        const QString body = line.mid(1).trimmed();
        if (body.isEmpty()) // the '#' separator line
            continue;
        if (body.startsWith("GENERATED") || body.startsWith("Regenerate"))
            continue; // the do-not-edit banner
        lines << body;
    }
    return lines.join("\n");
}

/**
 * @brief Filter a tree item (and its subtree) against @p needle.
 *
 * A script leaf is shown when its label or tooltip matches; a folder is
 * shown when any descendant matches, and is auto-expanded so the hit is
 * visible. Returns whether the subtree has any visible match.
 */
bool filter_item(QTreeWidgetItem* item, const QString& needle) {
    const bool is_leaf = !item->data(0, role_path).toString().isEmpty();
    if (is_leaf) {
        const bool match = needle.isEmpty() || item->text(0).toLower().contains(needle) ||
                           item->toolTip(0).toLower().contains(needle);
        item->setHidden(!match);
        return match;
    }

    bool any = false;
    for (int i = 0; i < item->childCount(); ++i)
        // Bitwise | (not ||): every child must be visited to update its
        // hidden state, so short-circuit evaluation would leave some
        // children stale-visible when the filter is cleared.
        any = filter_item(item->child(i), needle) | any;
    item->setHidden(!any);
    if (!needle.isEmpty() && any)
        item->setExpanded(true);
    return any;
}

}

ScriptLibraryPanel::ScriptLibraryPanel(QWidget* parent)
    : QWidget(parent) {
    setup_ui();
    refresh();
}

QString ScriptLibraryPanel::library_dir() {
    // 1. Explicit override (development, packaging, tests).
    const QString env = qEnvironmentVariable("ORES_SHELL_SCRIPTS_DIR");
    if (!env.isEmpty() && QDir(env).exists())
        return env;

    // 2. Installed location relative to the executable.
    const QDir app(QCoreApplication::applicationDirPath());
    const QString installed = app.filePath("../share/ores/shell-scripts");
    if (QDir(installed).exists())
        return QDir(installed).absolutePath();

    // 3. Development repo layout (running from a build tree): the
    // generated library folder, tangled from the shell recipes.
    QDir dev(QCoreApplication::applicationDirPath());
    for (int i = 0; i < 8; ++i) {
        const QString candidate = dev.filePath("projects/ores.shell/scripts/library");
        if (QDir(candidate).exists())
            return QDir(candidate).absolutePath();
        if (!dev.cdUp())
            break;
    }
    using namespace ores::logging;
    BOOST_LOG_SEV(lg(), warn) << "Script library directory not found; the Scripts panel will be "
                                 "empty. Set ORES_SHELL_SCRIPTS_DIR to override.";
    return {};
}

QString ScriptLibraryPanel::user_dir() {
    const QString base = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    const QString dir = QDir(base).filePath("scripts");
    if (!QDir().mkpath(dir)) {
        using namespace ores::logging;
        BOOST_LOG_SEV(lg(), warn) << "Could not create user scripts directory: "
                                  << dir.toStdString();
    }
    return dir;
}

void ScriptLibraryPanel::setup_ui() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(4);

    auto* header = new QLabel("Scripts", this);
    header->setStyleSheet("font-weight: bold; padding: 2px;");
    layout->addWidget(header);

    filter_ = new QLineEdit(this);
    filter_->setPlaceholderText("Filter scripts...");
    filter_->setClearButtonEnabled(true);
    connect(filter_, &QLineEdit::textChanged, this, &ScriptLibraryPanel::on_filter_changed);
    layout->addWidget(filter_);

    tree_ = new QTreeWidget(this);
    tree_->setHeaderHidden(true);
    tree_->setRootIsDecorated(true);
    tree_->setIconSize(QSize(16, 16));
    // Activating a row (double-click or Enter) opens it in the editor.
    connect(tree_, &QTreeWidget::itemActivated, this, &ScriptLibraryPanel::on_item_activated);
    // Right-click a script for Open / Execute.
    tree_->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(tree_,
            &QTreeWidget::customContextMenuRequested,
            this,
            &ScriptLibraryPanel::on_context_menu);
    layout->addWidget(tree_);
}

void ScriptLibraryPanel::add_scripts(QTreeWidgetItem* group, const QString& dir, bool library) {
    QDir d(dir);

    // Category sub-folders first, each a collapsible group, recursing so
    // the library mirrors its on-disk folder structure.
    const auto subdirs = d.entryInfoList(QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name);
    for (const auto& sub : subdirs) {
        auto* folder = new QTreeWidgetItem(group);
        folder->setText(0, sub.fileName());
        folder->setIcon(0,
                        IconUtils::createRecoloredIcon(Icon::Folder, IconUtils::DefaultIconColor));
        folder->setFlags(folder->flags() & ~Qt::ItemIsSelectable);
        add_scripts(folder, sub.absoluteFilePath(), library);
        folder->setExpanded(false);
    }

    // Then the scripts in this directory. The label keeps the .ores
    // extension; the script's purpose is a tooltip, not truncated text.
    const auto entries = d.entryInfoList({"*.ores"}, QDir::Files, QDir::Name);
    for (const auto& fi : entries) {
        auto* item = new QTreeWidgetItem(group);
        item->setText(0, fi.fileName());
        item->setIcon(
            0, IconUtils::createRecoloredIcon(Icon::DocumentCode, IconUtils::DefaultIconColor));
        item->setData(0, role_path, fi.absoluteFilePath());
        item->setData(0, role_library, library);
        const QString desc = description_for(fi.absoluteFilePath());
        item->setToolTip(
            0, desc.isEmpty() ? fi.absoluteFilePath() : desc + "\n\n" + fi.absoluteFilePath());
    }
}

void ScriptLibraryPanel::refresh() {
    tree_->clear();

    const QIcon folder = IconUtils::createRecoloredIcon(Icon::Folder, IconUtils::DefaultIconColor);

    auto* library = new QTreeWidgetItem(tree_);
    library->setText(0, "Library");
    library->setIcon(0, folder);
    library->setFlags(library->flags() & ~Qt::ItemIsSelectable);
    add_scripts(library, library_dir(), true);
    library->setExpanded(true);

    auto* mine = new QTreeWidgetItem(tree_);
    mine->setText(0, "My Scripts");
    mine->setIcon(0, folder);
    mine->setFlags(mine->flags() & ~Qt::ItemIsSelectable);
    add_scripts(mine, user_dir(), false);
    mine->setExpanded(true);

    on_filter_changed(filter_->text());
}

void ScriptLibraryPanel::on_filter_changed(const QString& text) {
    const QString needle = text.trimmed().toLower();
    // Recurse the whole tree so a match deep inside a category folder
    // surfaces (and the folder expands). A top-level group stays visible
    // when unfiltered; while filtering, an empty one is hidden too rather
    // than left as a dangling header.
    for (int g = 0; g < tree_->topLevelItemCount(); ++g) {
        auto* group = tree_->topLevelItem(g);
        const bool any = filter_item(group, needle);
        group->setHidden(!needle.isEmpty() && !any);
    }
}

void ScriptLibraryPanel::on_item_activated(QTreeWidgetItem* item, int /*column*/) {
    if (!item)
        return;
    const QString path = item->data(0, role_path).toString();
    if (path.isEmpty()) // a group header
        return;
    emit statusChanged(tr("Opening %1").arg(QFileInfo(path).fileName()));
    emit openRequested(path, item->data(0, role_library).toBool());
}

void ScriptLibraryPanel::on_context_menu(const QPoint& pos) {
    auto* item = tree_->itemAt(pos);
    if (!item)
        return;
    const QString path = item->data(0, role_path).toString();
    if (path.isEmpty()) // a folder / group, not a script
        return;
    const bool library = item->data(0, role_library).toBool();
    const QString name = QFileInfo(path).fileName();

    QMenu menu(this);
    auto* open = menu.addAction(
        IconUtils::createRecoloredIcon(Icon::Open, IconUtils::DefaultIconColor), tr("Open"));
    auto* execute = menu.addAction(
        IconUtils::createRecoloredIcon(Icon::Terminal, IconUtils::DefaultIconColor), tr("Execute"));

    const auto* chosen = menu.exec(tree_->viewport()->mapToGlobal(pos));
    if (chosen == open) {
        emit statusChanged(tr("Opening %1").arg(name));
        emit openRequested(path, library);
    } else if (chosen == execute) {
        // Execute runs the script straight away (no editor), and a
        // provisioning script mutates the system — confirm first.
        const auto answer =
            QMessageBox::question(this,
                                  tr("Execute script?"),
                                  tr("Run %1 in the shell now?\n\nIts commands are sent to the "
                                     "connected session and may change system state.")
                                      .arg(name),
                                  QMessageBox::Yes | QMessageBox::No,
                                  QMessageBox::No);
        if (answer == QMessageBox::Yes) {
            emit statusChanged(tr("Executing %1").arg(name));
            emit executeRequested(path);
        }
    }
}

}
