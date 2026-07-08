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
#include "ores.qt/ScriptEditorMdiWindow.hpp"
#include "ores.qt.headless/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ScriptHighlighter.hpp"
#include "ores.qt/ScriptLibraryPanel.hpp"
#include <QCloseEvent>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QInputDialog>
#include <QMessageBox>
#include <QTextStream>
#include <QVBoxLayout>

namespace ores::qt {

ScriptEditorMdiWindow::ScriptEditorMdiWindow(const QString& path, bool library, QWidget* parent)
    : QWidget(parent) {
    setup_ui();
    load_from(path, library);
}

void ScriptEditorMdiWindow::setup_ui() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    toolbar_ = new QToolBar(this);
    toolbar_->setIconSize(QSize(20, 20));

    // Run sends the script to the shell terminal (load <path>); the
    // terminal icon reads as "run in the shell".
    run_action_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Terminal, IconUtils::DefaultIconColor), tr("Run"));
    run_action_->setToolTip(tr("Run this script in the shell (load <path>)"));
    connect(run_action_, &QAction::triggered, this, &ScriptEditorMdiWindow::on_run);

    toolbar_->addSeparator();

    save_action_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor), tr("Save"));
    save_action_->setToolTip(tr("Save changes"));
    connect(save_action_, &QAction::triggered, this, &ScriptEditorMdiWindow::on_save);

    // Save As reuses the Copy icon: it creates a copy in My Scripts.
    save_as_action_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Copy, IconUtils::DefaultIconColor), tr("Save As"));
    save_as_action_->setToolTip(tr("Save a copy in My Scripts"));
    connect(save_as_action_, &QAction::triggered, this, &ScriptEditorMdiWindow::on_save_as);

    delete_action_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor), tr("Delete"));
    delete_action_->setToolTip(tr("Delete this script"));
    connect(delete_action_, &QAction::triggered, this, &ScriptEditorMdiWindow::on_delete);

    layout->addWidget(toolbar_);

    editor_ = new QPlainTextEdit(this);
    editor_->setFont(FontUtils::monospace());
    new ScriptHighlighter(editor_->document());
    connect(
        editor_, &QPlainTextEdit::textChanged, this, &ScriptEditorMdiWindow::on_editor_modified);
    layout->addWidget(editor_);
}

void ScriptEditorMdiWindow::load_from(const QString& path, bool library) {
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
    update_actions();
    update_title();
}

void ScriptEditorMdiWindow::update_actions() {
    const bool has = !current_path_.isEmpty();
    run_action_->setEnabled(has);
    // A library template is pristine: Save and Delete are disabled
    // (grayed), Save As makes an editable copy.
    save_action_->setEnabled(has && !current_is_library_ && dirty_);
    save_as_action_->setEnabled(has);
    delete_action_->setEnabled(has && !current_is_library_);
}

void ScriptEditorMdiWindow::update_title() {
    QString name =
        current_path_.isEmpty() ? QStringLiteral("Script") : QFileInfo(current_path_).fileName();
    if (dirty_)
        name += " *";
    // The MDI sub-window mirrors the widget's window title.
    setWindowTitle(QString("Script: %1").arg(name));
}

void ScriptEditorMdiWindow::on_editor_modified() {
    if (!dirty_) {
        dirty_ = true;
        update_actions();
        update_title();
    }
}

bool ScriptEditorMdiWindow::save_buffer_to(const QString& path) {
    QFile f(path);
    if (!f.open(QIODevice::WriteOnly | QIODevice::Text | QIODevice::Truncate)) {
        emit statusChanged(QString("Cannot write: %1").arg(path));
        return false;
    }
    QTextStream out(&f);
    out << editor_->toPlainText();
    out.flush();

    current_path_ = path;
    current_is_library_ = false;
    dirty_ = false;
    update_actions();
    update_title();
    emit statusChanged(QString("Saved %1").arg(path));
    emit libraryChanged();
    return true;
}

void ScriptEditorMdiWindow::on_save() {
    if (current_path_.isEmpty() || current_is_library_)
        return;
    (void)save_buffer_to(current_path_);
}

void ScriptEditorMdiWindow::on_save_as() {
    QString suggested = current_path_.isEmpty() ? QStringLiteral("my_script.ores") :
                                                  QFileInfo(current_path_).fileName();

    bool ok = false;
    QString name = QInputDialog::getText(
        this, "Save script as", "File name (in My Scripts):", QLineEdit::Normal, suggested, &ok);
    if (!ok || name.trimmed().isEmpty())
        return;
    if (!name.endsWith(".ores"))
        name += ".ores";
    (void)save_buffer_to(QDir(ScriptLibraryPanel::user_dir()).filePath(name));
}

void ScriptEditorMdiWindow::on_delete() {
    if (current_path_.isEmpty() || current_is_library_)
        return;
    const auto choice =
        QMessageBox::question(this,
                              "Delete script?",
                              QString("Delete %1?").arg(QFileInfo(current_path_).fileName()),
                              QMessageBox::Yes | QMessageBox::No);
    if (choice != QMessageBox::Yes)
        return;
    if (QFile::remove(current_path_)) {
        emit statusChanged(QString("Deleted %1").arg(current_path_));
        emit libraryChanged();
        close();
    }
}

void ScriptEditorMdiWindow::on_run() {
    if (current_path_.isEmpty())
        return;
    // load reads from disk, so a dirty buffer must be saved first. A
    // library template cannot be saved in place: promote it via Save As.
    if (dirty_) {
        if (current_is_library_) {
            on_save_as();
            if (dirty_) // user cancelled Save As
                return;
        } else if (!save_buffer_to(current_path_)) {
            return;
        }
    }
    emit runRequested(current_path_);
}

void ScriptEditorMdiWindow::closeEvent(QCloseEvent* event) {
    if (dirty_ && !current_is_library_) {
        const auto choice = QMessageBox::question(this,
                                                  "Discard changes?",
                                                  QString("%1 has unsaved changes. Discard them?")
                                                      .arg(QFileInfo(current_path_).fileName()),
                                                  QMessageBox::Discard | QMessageBox::Cancel);
        if (choice == QMessageBox::Cancel) {
            event->ignore();
            return;
        }
    }
    event->accept();
}

}
