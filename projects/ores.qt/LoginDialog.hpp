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
#ifndef ORES_QT_LOGINDIALOG_HPP
#define ORES_QT_LOGINDIALOG_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <memory>
#include <thread>
#include <boost/asio/io_context.hpp>
#include <boost/asio/executor_work_guard.hpp>
#include <QLabel>
#include <QDialog>
#include <QLineEdit>
#include <QSpinBox>
#include <QPushButton>
#include "ores.comms/client.hpp"

namespace ores::qt {

/**
 * @brief Dialog for user authentication and server connection.
 *
 * Presents a login form with username, password, server host, and port fields.
 * Handles async connection to the ores.comms server and performs login.
 */
class LoginDialog : public QDialog {
    Q_OBJECT

private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.comms.login_dialog");
        return instance;
    }

public:
    explicit LoginDialog(QWidget* parent = nullptr);
    ~LoginDialog() override;

    /**
     * @brief Get the connected client instance.
     * @return Shared pointer to the connected client, or nullptr if login failed.
     */
    std::shared_ptr<comms::client> getClient() const { return client_; }

    /**
     * @brief Get the logged-in username.
     * @return The username used for login.
     */
    std::string getUsername() const { return username_edit_->text().toStdString(); }

    /**
     * @brief Get the IO context for async operations.
     * @return Unique pointer to the IO context.
     */
    std::unique_ptr<boost::asio::io_context> takeIOContext() {
        return std::move(io_context_);
    }

    /**
     * @brief Get the IO thread for async operations.
     * @return Unique pointer to the IO thread.
     */
    std::unique_ptr<std::thread> takeIOThread() {
        return std::move(io_thread_);
    }

    /**
     * @brief Get the work guard for the IO context.
     * @return Unique pointer to the work guard.
     */
    std::unique_ptr<boost::asio::executor_work_guard<
        boost::asio::io_context::executor_type>>
    takeWorkGuard() {
        return std::move(work_guard_);
    }

private slots:
    void onLoginClicked();
    void onConnectionResult(bool success, const QString& error_message);
    void onLoginResult(bool success, const QString& error_message);

signals:
    void connectionCompleted(bool success, const QString& error_message);
    void loginCompleted(bool success, const QString& error_message);

private:
    void setupUI();
    void enableForm(bool enabled);
    void performLogin(const std::string& username, const std::string& password);

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
    std::unique_ptr<boost::asio::executor_work_guard<
        boost::asio::io_context::executor_type>> work_guard_;
    std::unique_ptr<std::thread> io_thread_;
    std::shared_ptr<comms::client> client_;
};

}

#endif
