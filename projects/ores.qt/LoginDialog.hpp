/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_LOGIN_DIALOG_HPP
#define ORES_QT_LOGIN_DIALOG_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <QDialog>
#include <QLineEdit>
#include <QPushButton>
#include <QLabel>
#include <QSpinBox>
#include <memory>
#include <thread>
#include <boost/asio/io_context.hpp>
#include <boost/asio/executor_work_guard.hpp>
#include "ores.comms/client.hpp"

namespace ores::qt {

/**
 * @brief Dialog for user authentication and server connection.
 *
 * Presents a login form with username, password, server host, and port fields.
 * Handles async connection to the ores.comms server and performs login.
 */
class login_dialog : public QDialog {
    Q_OBJECT

public:
    explicit login_dialog(QWidget* parent = nullptr);
    ~login_dialog();

    /**
     * @brief Get the connected client instance.
     * @return Shared pointer to the connected client, or nullptr if login failed.
     */
    std::shared_ptr<comms::client> get_client() const { return client_; }

    /**
     * @brief Get the IO context for async operations.
     * @return Unique pointer to the IO context.
     */
    std::unique_ptr<boost::asio::io_context> take_io_context() {
        return std::move(io_context_);
    }

    /**
     * @brief Get the IO thread for async operations.
     * @return Unique pointer to the IO thread.
     */
    std::unique_ptr<std::thread> take_io_thread() {
        return std::move(io_thread_);
    }

    /**
     * @brief Get the work guard for the IO context.
     * @return Unique pointer to the work guard.
     */
    std::unique_ptr<boost::asio::executor_work_guard<boost::asio::io_context::executor_type>> take_work_guard() {
        return std::move(work_guard_);
    }

private slots:
    void on_login_clicked();
    void on_connection_result(bool success, const QString& error_message);
    void on_login_result(bool success, const QString& error_message);

signals:
    void connection_completed(bool success, const QString& error_message);
    void login_completed(bool success, const QString& error_message);

private:
    void setup_ui();
    void enable_form(bool enabled);
    void perform_login(const std::string& username, const std::string& password);

private:
    // UI components
    QLineEdit* username_edit_;
    QLineEdit* password_edit_;
    QLineEdit* host_edit_;
    QSpinBox* port_spinbox_;
    QPushButton* login_button_;
    QPushButton* cancel_button_;
    QLabel* status_label_;

    // Client infrastructure
    std::unique_ptr<boost::asio::io_context> io_context_;
    std::unique_ptr<boost::asio::executor_work_guard<boost::asio::io_context::executor_type>> work_guard_;
    std::unique_ptr<std::thread> io_thread_;
    std::shared_ptr<comms::client> client_;
};

}

#endif
