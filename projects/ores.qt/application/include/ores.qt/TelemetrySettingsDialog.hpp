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
#ifndef ORES_QT_TELEMETRYSETTINGSDIALOG_HPP
#define ORES_QT_TELEMETRYSETTINGSDIALOG_HPP

#include <QDialog>
#include <QCheckBox>
#include <QComboBox>
#include <QLineEdit>
#include <QSpinBox>
#include <QPushButton>
#include <QTabWidget>
#include "ores.logging/make_logger.hpp"
#include "ores.logging/logging_options.hpp"

namespace ores::qt {

/**
 * @brief Dialog for configuring logging and telemetry settings.
 *
 * Allows users to configure:
 * - Logging: level, console output, file output, directory, PID inclusion, tag filter
 * - Telemetry Export: output file, directory, streaming, batch size, flush interval
 *
 * Settings are persisted via QSettings and applied on next application restart.
 * Some settings (like log level) may be applied immediately if the runtime supports it.
 *
 * This dialog is accessed via Telemetry > Settings... menu item.
 */
class TelemetrySettingsDialog : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.telemetry_settings_dialog";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct TelemetrySettingsDialog.
     * @param parent Parent widget.
     */
    explicit TelemetrySettingsDialog(QWidget* parent = nullptr);
    ~TelemetrySettingsDialog() override;

    /**
     * @brief Load logging options from QSettings.
     * @return Logging options struct populated from settings.
     */
    static logging::logging_options loadLoggingSettings();

    /**
     * @brief Check if compression is enabled in QSettings.
     * @return True if compression is enabled.
     */
    static bool isCompressionEnabled();

    /**
     * @brief Get compression algorithm from QSettings.
     * @return Algorithm string (zlib, gzip, bzip2, or all).
     */
    static QString compressionAlgorithm();

    /**
     * @brief Check if telemetry streaming to server is enabled in QSettings.
     * @return True if streaming is enabled.
     */
    static bool isStreamingEnabled();

    /**
     * @brief Get streaming batch size from QSettings.
     * @return Batch size (default 50).
     */
    static int streamingBatchSize();

    /**
     * @brief Get streaming flush interval from QSettings.
     * @return Flush interval in seconds (default 5).
     */
    static int streamingFlushInterval();

    /**
     * @brief Get the settings key prefix for telemetry settings.
     * @return The settings key prefix.
     */
    static QString settingsPrefix() { return "telemetry"; }

private slots:
    void onApplyClicked();
    void onCancelClicked();
    void onBrowseLogDirectoryClicked();
    void onBrowseTelemetryDirectoryClicked();
    void onLoggingEnabledChanged(Qt::CheckState state);
    void onTelemetryExportEnabledChanged(Qt::CheckState state);
    void onCompressionEnabledChanged(Qt::CheckState state);

private:
    void setupUI();
    void loadSettings();
    void saveSettings();
    void applySettings();
    void updateLoggingGroupEnabled();
    void updateTelemetryGroupEnabled();
    void updateCompressionGroupEnabled();

private:
    // Tab widget
    QTabWidget* tab_widget_;

    // Logging tab
    QCheckBox* logging_enabled_checkbox_;
    QComboBox* log_level_combo_;
    QCheckBox* console_output_checkbox_;
    QLineEdit* log_directory_edit_;
    QPushButton* log_directory_browse_;
    QLineEdit* log_filename_edit_;
    QCheckBox* include_pid_checkbox_;
    QLineEdit* tag_filter_edit_;

    // Telemetry export tab
    QCheckBox* telemetry_enabled_checkbox_;
    QLineEdit* telemetry_output_file_edit_;
    QLineEdit* telemetry_directory_edit_;
    QPushButton* telemetry_directory_browse_;
    QCheckBox* streaming_enabled_checkbox_;
    QSpinBox* batch_size_spin_;
    QSpinBox* flush_interval_spin_;

    // Network tab
    QCheckBox* compression_enabled_checkbox_;
    QComboBox* compression_algorithm_combo_;

    // Dialog buttons
    QPushButton* apply_button_;
    QPushButton* cancel_button_;
};

}

#endif
