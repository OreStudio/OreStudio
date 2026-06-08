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
#ifndef ORES_QT_SCRIPT_LIBRARY_PANEL_HPP
#define ORES_QT_SCRIPT_LIBRARY_PANEL_HPP

#include "ores.logging/make_logger.hpp"
#include <QLabel>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QString>
#include <QTreeWidget>
#include <QWidget>

namespace ores::qt {

/**
 * @brief Browse, edit, and run ores-shell scripts.
 *
 * Two sources are shown side by side. /Library/ scripts are the
 * shipped =.ores= files tangled from literate org sources — they are
 * build artefacts and stay pristine: a library script opens in the
 * editor as a template, and Save on one redirects to Save As into the
 * user scripts area. /My Scripts/ are the operator's own =.ores=
 * files in a writable per-user directory; they edit, save in place
 * and delete. Copying a library script and tweaking it is the primary
 * creation flow.
 *
 * The panel never executes anything itself: Run saves a dirty buffer
 * and emits runRequested() with the script path, which the embedding
 * shell window feeds to the REPL as =load <path>=, so stop-on-error
 * semantics and the shell's live output come for free.
 */
class ScriptLibraryPanel : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.script_library_panel";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ScriptLibraryPanel(QWidget* parent = nullptr);

    /**
     * @brief Re-scan both script directories and rebuild the tree.
     */
    void refresh();

    /**
     * @brief Absolute path of the shipped library scripts directory.
     *
     * Resolved, in order: the =ORES_SHELL_SCRIPTS_DIR= environment
     * variable; =<app-dir>/../share/ores/shell-scripts=; and the
     * development repo path =projects/ores.shell/scripts=.
     */
    static QString library_dir();

    /**
     * @brief Absolute path of the writable per-user scripts directory
     * (created on first use): =<AppDataLocation>/scripts=.
     */
    static QString user_dir();

signals:
    /**
     * @brief The user asked to run the script at @p path. The shell
     * window issues =load <path>= for it.
     */
    void runRequested(const QString& path);

    /// Status-bar text for the embedding window.
    void statusChanged(const QString& message);

private slots:
    void on_selection_changed();
    void on_filter_changed(const QString& text);
    void on_run();
    void on_save();
    void on_save_as();
    void on_delete();
    void on_editor_modified();

private:
    void setup_ui();
    void add_scripts(QTreeWidgetItem* group, const QString& dir, bool library);
    void load_into_editor(const QString& path, bool library);
    [[nodiscard]] bool save_buffer_to(const QString& path);
    void update_button_state();
    void update_editor_header();

    QLineEdit* filter_{nullptr};
    QTreeWidget* tree_{nullptr};
    QLabel* editor_header_{nullptr};
    QPlainTextEdit* editor_{nullptr};
    QPushButton* run_button_{nullptr};
    QPushButton* save_button_{nullptr};
    QPushButton* save_as_button_{nullptr};
    QPushButton* delete_button_{nullptr};

    /// Path of the script currently in the editor, empty if none.
    QString current_path_;
    /// True when the current script is a read-only library template.
    bool current_is_library_{false};
    /// True when the editor buffer has unsaved changes.
    bool dirty_{false};
};

}

#endif
