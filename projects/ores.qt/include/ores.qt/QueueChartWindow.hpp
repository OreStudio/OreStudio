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
#ifndef ORES_QT_QUEUE_CHART_WINDOW_HPP
#define ORES_QT_QUEUE_CHART_WINDOW_HPP

#include <QWidget>
#include <QComboBox>
#include <QToolBar>
#include <QFutureWatcher>
#include <QtCharts/QChartView>
#include <chrono>
#include <optional>
#include <vector>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.mq/pgmq/metrics_sample.hpp"

namespace ores::qt {

/**
 * @brief MDI window showing time-series charts for a single pgmq queue.
 *
 * Displays queue_length and total_messages over a configurable time window.
 * Fetches data from ores_mq_metrics_samples_tbl via the
 * get_queue_metric_samples_request protocol message.
 */
class QueueChartWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.queue_chart_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    enum class TimeRange {
        LastHour,
        Last6Hours,
        Last24Hours,
        Last7Days,
        AllTime
    };

public:
    explicit QueueChartWindow(const QString& queueName,
                              ClientManager* clientManager,
                              QWidget* parent = nullptr);
    ~QueueChartWindow() override = default;

    [[nodiscard]] const QString& queueName() const { return queueName_; }

    QSize sizeHint() const override { return {900, 500}; }

    void reload();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private slots:
    void onDataLoaded();
    void onTimeRangeChanged(int index);

private:
    void setupUi();
    void setupToolbar();
    void setupChart();

    void populateChart(const std::vector<mq::pgmq::metrics_sample>& samples);
    void clearChart(const QString& message);

    std::optional<std::chrono::system_clock::time_point> fromForRange() const;

    struct FetchResult {
        bool success{false};
        std::vector<mq::pgmq::metrics_sample> samples;
        QString error_message;
        QString error_details;
    };

    QString queueName_;
    ClientManager* clientManager_;
    TimeRange currentRange_{TimeRange::LastHour};

    QToolBar* toolbar_;
    QAction* reloadAction_;
    QComboBox* rangeCombo_;
    QChartView* chartView_;

    QFutureWatcher<FetchResult>* watcher_;
    bool isFetching_{false};
};

}

#endif
