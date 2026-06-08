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
#include "ores.qt/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ScriptHighlighter.hpp"
#include <QCoreApplication>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QLabel>
#include <QMessageBox>
#include <QSplitter>
#include <QStandardPaths>
#include <QTextStream>
#include <QVBoxLayout>

namespace ores::qt {

namespace {

constexpr int role_path = Qt::UserRole;
constexpr int role_library = Qt::UserRole + 1;

/**
 * @brief Read the one-line description of a script from its sibling
 * literate org source (the #+description: or #+title: keyword), so
 * the tree shows what each .ores does. Empty when there is no source.
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
    update_button_state();
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

    // The panel is a left sidebar: the script list sits above the
    // editor so both have full width in a narrow column.
    auto* splitter = new QSplitter(Qt::Vertical, this);

    // Top: a titled "Scripts" pane with a filter above the tree.
    auto* tree_pane = new QWidget(splitter);
    auto* tree_layout = new QVBoxLayout(tree_pane);
    tree_layout->setContentsMargins(0, 0, 0, 0);
    tree_layout->setSpacing(4);

    auto* tree_header = new QLabel("Scripts", tree_pane);
    tree_header->setStyleSheet("font-weight: bold; padding: 2px;");
    tree_layout->addWidget(tree_header);

    filter_ = new QLineEdit(tree_pane);
    filter_->setPlaceholderText("Filter scripts...");
    filter_->setClearButtonEnabled(true);
    connect(filter_, &QLineEdit::textChanged, this,
            &ScriptLibraryPanel::on_filter_changed);
    tree_layout->addWidget(filter_);

    tree_ = new QTreeWidget(tree_pane);
    tree_->setHeaderHidden(true);
    tree_->setRootIsDecorated(true);
    connect(tree_, &QTreeWidget::itemSelectionChanged, this,
            &ScriptLibraryPanel::on_selection_changed);
    tree_layout->addWidget(tree_);

    // Right: a titled editor pane — the header names the open script
    // and its state (read-only template, or modified).
    auto* editor_pane = new QWidget(splitter);
    auto* editor_layout = new QVBoxLayout(editor_pane);
    editor_layout->setContentsMargins(0, 0, 0, 0);
    editor_layout->setSpacing(4);

    editor_header_ = new QLabel("No script selected", editor_pane);
    editor_header_->setStyleSheet("font-weight: bold; padding: 2px;");
    editor_layout->addWidget(editor_header_);

    editor_ = new QPlainTextEdit(editor_pane);
    editor_->setFont(FontUtils::monospace());
    editor_->setPlaceholderText("Select a script to view or edit.");
    new ScriptHighlighter(editor_->document());
    connect(editor_, &QPlainTextEdit::textChanged, this,
            &ScriptLibraryPanel::on_editor_modified);
    editor_layout->addWidget(editor_);

    auto* buttons = new QHBoxLayout();
    run_button_ = new QPushButton("Run", editor_pane);
    run_button_->setToolTip("Run this script in the shell below (load <path>)");
    save_button_ = new QPushButton("Save", editor_pane);
    save_as_button_ = new QPushButton("Save As...", editor_pane);
    delete_button_ = new QPushButton("Delete", editor_pane);
    connect(run_button_, &QPushButton::clicked, this, &ScriptLibraryPanel::on_run);
    connect(save_button_, &QPushButton::clicked, this, &ScriptLibraryPanel::on_save);
    connect(save_as_button_, &QPushButton::clicked, this,
            &ScriptLibraryPanel::on_save_as);
    connect(delete_button_, &QPushButton::clicked, this,
            &ScriptLibraryPanel::on_delete);
    buttons->addWidget(run_button_);
    buttons->addStretch();
    buttons->addWidget(save_button_);
    buttons->addWidget(save_as_button_);
    buttons->addWidget(delete_button_);
    editor_layout->addLayout(buttons);

    splitter->addWidget(tree_pane);
    splitter->addWidget(editor_pane);
    splitter->setStretchFactor(0, 1);
    splitter->setStretchFactor(1, 2);
    layout->addWidget(splitter);
}

void ScriptLibraryPanel::add_scripts(QTreeWidgetItem* group, const QString& dir,
                                     bool library) {
    QDir d(dir);
    const auto entries = d.entryInfoList({"*.ores"}, QDir::Files, QDir::Name);
    for (const auto& fi : entries) {
        auto* item = new QTreeWidgetItem(group);
        const QString desc = description_for(fi.absoluteFilePath());
        item->setText(0, desc.isEmpty() ? fi.fileName()
                                        : fi.fileName() + "  —  " + desc);
        item->setIcon(0, IconUtils::createRecoloredIcon(Icon::DocumentCode,
                                                        IconUtils::DefaultIconColor));
        item->setData(0, role_path, fi.absoluteFilePath());
        item->setData(0, role_library, library);
        item->setToolTip(0, fi.absoluteFilePath());
    }
}

void ScriptLibraryPanel::refresh() {
    tree_->clear();

    const QIcon folder = IconUtils::createRecoloredIcon(Icon::Folder,
                                                        IconUtils::DefaultIconColor);

    auto* library = new QTreeWidgetItem(tree_);
    library->setText(0, "Library (read-only)");
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
            const bool match =
                needle.isEmpty() || item->text(0).toLower().contains(needle);
            item->setHidden(!match);
        }
    }
}

void ScriptLibraryPanel::load_into_editor(const QString& path, bool library) {
    QFile f(path);
    if (!f.open(QIODevice::ReadOnly | QIODevice::Text)) {
        emit statusChanged(QString("Cannot open script: %1").arg(path));
        return;
    }
    QTextStream in(&f);
    const QString contents = in.readAll();

    // Block textChanged so loading does not mark the buffer dirty.
    const QSignalBlocker blocker(editor_);
    editor_->setPlainText(contents);

    current_path_ = path;
    current_is_library_ = library;
    dirty_ = false;
    update_button_state();
    update_editor_header();
}

void ScriptLibraryPanel::update_editor_header() {
    if (current_path_.isEmpty()) {
        editor_header_->setText("No script selected");
        return;
    }
    QString name = QFileInfo(current_path_).fileName();
    if (current_is_library_)
        name += "  (library template — Save creates a copy)";
    else if (dirty_)
        name += "  (modified)";
    editor_header_->setText(name);
}

void ScriptLibraryPanel::on_selection_changed() {
    auto items = tree_->selectedItems();
    if (items.isEmpty())
        return;
    auto* item = items.first();
    const QString path = item->data(0, role_path).toString();
    if (path.isEmpty())
        return;

    if (dirty_) {
        const auto choice = QMessageBox::question(
            this, "Discard changes?",
            "The current script has unsaved changes. Discard them?",
            QMessageBox::Discard | QMessageBox::Cancel);
        if (choice == QMessageBox::Cancel)
            return;
    }
    load_into_editor(path, item->data(0, role_library).toBool());
}

void ScriptLibraryPanel::on_editor_modified() {
    if (!current_path_.isEmpty() && !dirty_) {
        dirty_ = true;
        update_button_state();
    }
}

bool ScriptLibraryPanel::save_buffer_to(const QString& path) {
    QFile f(path);
    if (!f.open(QIODevice::WriteOnly | QIODevice::Text | QIODevice::Truncate)) {
        emit statusChanged(QString("Cannot write: %1").arg(path));
        return false;
    }
    QTextStream out(&f);
    out << editor_->toPlainText();
    current_path_ = path;
    current_is_library_ = false;
    dirty_ = false;
    refresh();
    update_button_state();
    update_editor_header();
    emit statusChanged(QString("Saved %1").arg(path));
    return true;
}

void ScriptLibraryPanel::on_save() {
    if (current_path_.isEmpty())
        return;
    // Library scripts are pristine build artefacts: editing one is
    // really "start a new user script from this template", so Save
    // becomes Save As into the user area.
    if (current_is_library_) {
        on_save_as();
        return;
    }
    (void)save_buffer_to(current_path_);
}

void ScriptLibraryPanel::on_save_as() {
    QString suggested = "my_script.ores";
    if (!current_path_.isEmpty())
        suggested = QFileInfo(current_path_).fileName();

    bool ok = false;
    QString name = QInputDialog::getText(
        this, "Save script as", "File name (in My Scripts):", QLineEdit::Normal,
        suggested, &ok);
    if (!ok || name.trimmed().isEmpty())
        return;
    if (!name.endsWith(".ores"))
        name += ".ores";
    (void)save_buffer_to(QDir(user_dir()).filePath(name));
}

void ScriptLibraryPanel::on_delete() {
    if (current_path_.isEmpty() || current_is_library_)
        return;
    const auto choice = QMessageBox::question(
        this, "Delete script?",
        QString("Delete %1?").arg(QFileInfo(current_path_).fileName()),
        QMessageBox::Yes | QMessageBox::No);
    if (choice != QMessageBox::Yes)
        return;
    if (QFile::remove(current_path_)) {
        emit statusChanged(QString("Deleted %1").arg(current_path_));
        editor_->clear();
        current_path_.clear();
        dirty_ = false;
        refresh();
        update_button_state();
        update_editor_header();
    }
}

void ScriptLibraryPanel::on_run() {
    if (current_path_.isEmpty())
        return;
    // A dirty buffer must be on disk before load reads it; for a
    // library template that means promoting it to a user script first.
    if (dirty_) {
        if (current_is_library_) {
            on_save_as();
            if (dirty_)  // user cancelled Save As
                return;
        } else if (!save_buffer_to(current_path_)) {
            return;
        }
    }
    emit runRequested(current_path_);
}

void ScriptLibraryPanel::update_button_state() {
    const bool has = !current_path_.isEmpty();
    run_button_->setEnabled(has);
    save_button_->setEnabled(has && (dirty_ || current_is_library_));
    save_as_button_->setEnabled(has);
    delete_button_->setEnabled(has && !current_is_library_);
}

}
