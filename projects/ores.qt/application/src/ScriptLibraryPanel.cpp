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
#include <QStandardPaths>
#include <QTextStream>
#include <QVBoxLayout>

namespace ores::qt {

namespace {

constexpr int role_path = Qt::UserRole;
constexpr int role_library = Qt::UserRole + 1;

/**
 * @brief Read the one-line description of a script from its sibling
 * literate org source (the #+description: or #+title: keyword), so the
 * tree can show what each .ores does as a tooltip. Empty when there is
 * no source.
 */
QString description_for(const QString& ores_path) {
    QFileInfo fi(ores_path);
    const QString org = fi.dir().filePath(fi.completeBaseName() + ".org");
    QFile f(org);
    if (!f.open(QIODevice::ReadOnly | QIODevice::Text))
        return {};

    QString title;
    QString description;
    QTextStream in(&f);
    while (!in.atEnd()) {
        const QString line = in.readLine();
        if (line.startsWith("#+description:"))
            description = line.mid(QString("#+description:").size()).trimmed();
        else if (line.startsWith("#+title:"))
            title = line.mid(QString("#+title:").size()).trimmed();
        if (!description.isEmpty())
            break;
    }
    return !description.isEmpty() ? description : title;
}

}

ScriptLibraryPanel::ScriptLibraryPanel(QWidget* parent) : QWidget(parent) {
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

    // 3. Development repo layout (running from a build tree).
    QDir dev(QCoreApplication::applicationDirPath());
    for (int i = 0; i < 8; ++i) {
        const QString candidate = dev.filePath("projects/ores.shell/scripts");
        if (QDir(candidate).exists())
            return QDir(candidate).absolutePath();
        if (!dev.cdUp())
            break;
    }
    return {};
}

QString ScriptLibraryPanel::user_dir() {
    const QString base =
        QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    const QString dir = QDir(base).filePath("scripts");
    QDir().mkpath(dir);
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
    connect(filter_, &QLineEdit::textChanged, this,
            &ScriptLibraryPanel::on_filter_changed);
    layout->addWidget(filter_);

    tree_ = new QTreeWidget(this);
    tree_->setHeaderHidden(true);
    tree_->setRootIsDecorated(true);
    tree_->setIconSize(QSize(16, 16));
    // Activating a row (double-click or Enter) opens it in the editor.
    connect(tree_, &QTreeWidget::itemActivated, this,
            &ScriptLibraryPanel::on_item_activated);
    layout->addWidget(tree_);
}

void ScriptLibraryPanel::add_scripts(QTreeWidgetItem* group, const QString& dir,
                                     bool library) {
    QDir d(dir);
    const auto entries = d.entryInfoList({"*.ores"}, QDir::Files, QDir::Name);
    for (const auto& fi : entries) {
        auto* item = new QTreeWidgetItem(group);
        // The label is just the file name; the script's purpose is a
        // tooltip rather than truncated inline text.
        item->setText(0, fi.fileName());
        item->setIcon(0, IconUtils::createRecoloredIcon(Icon::DocumentCode,
                                                        IconUtils::DefaultIconColor));
        item->setData(0, role_path, fi.absoluteFilePath());
        item->setData(0, role_library, library);
        const QString desc = description_for(fi.absoluteFilePath());
        item->setToolTip(0, desc.isEmpty() ? fi.absoluteFilePath()
                                           : desc + "\n" + fi.absoluteFilePath());
    }
}

void ScriptLibraryPanel::refresh() {
    tree_->clear();

    const QIcon folder = IconUtils::createRecoloredIcon(Icon::Folder,
                                                        IconUtils::DefaultIconColor);

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
    for (int g = 0; g < tree_->topLevelItemCount(); ++g) {
        auto* group = tree_->topLevelItem(g);
        for (int i = 0; i < group->childCount(); ++i) {
            auto* item = group->child(i);
            const bool match = needle.isEmpty() ||
                item->text(0).toLower().contains(needle) ||
                item->toolTip(0).toLower().contains(needle);
            item->setHidden(!match);
        }
    }
}

void ScriptLibraryPanel::on_item_activated(QTreeWidgetItem* item, int /*column*/) {
    if (!item)
        return;
    const QString path = item->data(0, role_path).toString();
    if (path.isEmpty())  // a group header
        return;
    emit openRequested(path, item->data(0, role_library).toBool());
}

}
