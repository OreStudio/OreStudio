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
#ifndef ORES_QT_SCRIPT_EDITOR_MDI_WINDOW_HPP
#define ORES_QT_SCRIPT_EDITOR_MDI_WINDOW_HPP

#include "ores.logging/make_logger.hpp"
#include <QAction>
#include <QPlainTextEdit>
#include <QString>
#include <QToolBar>
#include <QWidget>

namespace ores::qt {

/**
 * @brief Standalone editor for one ores-shell (.ores) script.
 *
 * An MDI child with the standard ORE Studio window anatomy: an icon
 * toolbar (Run, Save, Save As, Delete) over a monospace editor with
 * ores-shell syntax highlighting. The read-only nature of a /Library/
 * template is expressed by disabling Save (and Delete) rather than with
 * text: editing a template and Save As creates an editable copy in the
 * user scripts area.
 *
 * The window never executes anything itself — Run saves a dirty,
 * writable buffer and emits runRequested() with the script path, which
 * the shell window feeds to its REPL as =load <path>=.
 */
class ScriptEditorMdiWindow : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.script_editor_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Open @p path in the editor. @p library marks a read-only
     * library template (Save and Delete disabled; Save As promotes it to
     * a user script).
     */
    explicit ScriptEditorMdiWindow(const QString& path, bool library, QWidget* parent = nullptr);

    /// Absolute path of the script currently in the editor.
    [[nodiscard]] QString path() const {
        return current_path_;
    }

signals:
    /// Run the script at @p path (the shell window issues =load <path>=).
    void runRequested(const QString& path);

    /// A save or delete changed the script library; refresh browsers.
    void libraryChanged();

    /// Status-bar text for the embedding window.
    void statusChanged(const QString& message);

protected:
    void closeEvent(QCloseEvent* event) override;

private slots:
    void on_run();
    void on_save();
    void on_save_as();
    void on_delete();
    void on_editor_modified();

private:
    void setup_ui();
    void load_from(const QString& path, bool library);
    [[nodiscard]] bool save_buffer_to(const QString& path);
    void update_actions();
    void update_title();

    QToolBar* toolbar_{nullptr};
    QAction* run_action_{nullptr};
    QAction* save_action_{nullptr};
    QAction* save_as_action_{nullptr};
    QAction* delete_action_{nullptr};
    QPlainTextEdit* editor_{nullptr};

    QString current_path_;
    bool current_is_library_{false};
    bool dirty_{false};
};

}

#endif
