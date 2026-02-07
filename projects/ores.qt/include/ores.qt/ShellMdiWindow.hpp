/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_SHELL_MDI_WINDOW_HPP
#define ORES_QT_SHELL_MDI_WINDOW_HPP

#include <deque>
#include <mutex>
#include <thread>
#include <string>
#include <vector>
#include <memory>
#include <streambuf>
#include <condition_variable>
#include <QColor>
#include <QWidget>
#include <QToolBar>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QVBoxLayout>
#include <QCloseEvent>
#include "ores.qt/ClientManager.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.comms.shell/app/repl.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Stream buffer that emits Qt signals when text is written.
 *
 * Buffers characters and emits text_ready() on newline or sync().
 * Thread-safe via mutex.
 */
class qt_output_streambuf : public QObject, public std::streambuf {
    Q_OBJECT

public:
    explicit qt_output_streambuf(QObject* parent = nullptr);

signals:
    void text_ready(const QString& text);

protected:
    int_type overflow(int_type ch) override;
    int sync() override;

private:
    void flush_buffer();

    std::mutex mutex_;
    std::string buffer_;
};

/**
 * @brief Stream buffer that blocks on read until data is fed from the UI.
 *
 * Uses a queue of lines. underflow() blocks on a condition variable until
 * feed_line() provides data or close() sets the EOF flag.
 */
class qt_input_streambuf : public std::streambuf {
public:
    qt_input_streambuf() = default;

    /**
     * @brief Feed a line of input (called from UI thread).
     */
    void feed_line(const std::string& line);

    /**
     * @brief Signal EOF to unblock the reader.
     */
    void close();

protected:
    int_type underflow() override;

private:
    std::mutex mutex_;
    std::condition_variable cv_;
    std::deque<std::string> pending_;
    std::string current_;
    bool closed_{false};
};

/**
 * @brief MDI window embedding the ores.comms.shell REPL.
 *
 * Provides a toolbar, read-only output area, and a command input line.
 * Maintains its own independent server connection, auto-connecting and
 * logging in using credentials from the active Qt session.
 */
class ShellMdiWindow : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.shell_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ShellMdiWindow(ClientManager* clientManager,
                            QWidget* parent = nullptr);
    ~ShellMdiWindow() override;

signals:
    void statusChanged(const QString& message);

protected:
    void closeEvent(QCloseEvent* event) override;

private slots:
    void on_command_entered();
    void on_output_ready(const QString& text);
    void on_load_script();
    void on_save_script();

private:
    void setup_ui();
    void start_shell();
    void stop_shell();
    void on_repl_finished();

    ClientManager* client_manager_;
    QToolBar* toolbar_;
    QPlainTextEdit* output_area_;
    QLineEdit* input_line_;

    std::unique_ptr<qt_output_streambuf> output_buf_;
    std::unique_ptr<qt_input_streambuf> input_buf_;
    std::unique_ptr<std::ostream> out_stream_;
    std::unique_ptr<std::istream> in_stream_;

    std::unique_ptr<comms::net::client_session> shell_session_;
    std::unique_ptr<comms::shell::app::repl> shell_repl_;
    std::unique_ptr<std::thread> worker_thread_;

    // Soft violet for prompt, teal-blue for user input
    QColor prompt_color_{0xB4, 0x8E, 0xAD};
    QColor input_color_{0x88, 0xC0, 0xD0};

    // History of commands entered by the user for Save
    std::vector<std::string> command_history_;
};

}

#endif
