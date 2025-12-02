#include <iostream>
#include <boost/asio.hpp>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/bind_cancellation_slot.hpp>
#include <memory>
#include <vector>
#include <syncstream>  // C++23 synchronized output

namespace asio = boost::asio;
using asio::ip::tcp;
using asio::cancellation_type;
using asio::cancellation_slot;
using asio::cancellation_signal;

// Global counters for testing
std::atomic<int> sessions_created{0};
std::atomic<int> sessions_cancelled{0};

class session : public std::enable_shared_from_this<session>
{
public:
    session(tcp::socket socket, cancellation_signal& cancel_signal)
        : socket_(std::move(socket))
        , cancel_signal_(cancel_signal)
        , session_id_(++sessions_created)
    {
        std::osyncstream(std::cout)
            << "*** Session " << session_id_ << " (ptr=" << this
            << ") CREATED\n";

        // Connect this session to the global cancellation signal
        slot_ = cancel_signal_.slot();

        std::osyncstream(std::cout)
            << "*** Session " << session_id_ << " slot.is_connected()="
            << slot_.is_connected() << "\n";

        slot_.assign([this](cancellation_type type) {
            handle_cancellation(type);
        });

        std::osyncstream(std::cout)
            << "*** Session " << session_id_ << " handler assigned\n";
    }

    ~session()
    {
        std::osyncstream(std::cout)
            << "*** Session " << session_id_ << " DESTROYED\n";

        // Clean up the slot when session is destroyed
        if (slot_.is_connected()) {
            slot_.clear();
        }
    }

    void start()
    {
        active_ = true;
        std::osyncstream(std::cout)
            << "*** Session " << session_id_ << " STARTED\n";
        do_read();
    }

    int session_id() const { return session_id_; }

private:
    void handle_cancellation(cancellation_type type)
    {
        std::osyncstream(std::cout)
            << "!!! HANDLER CALLED for Session " << session_id_
            << " type=" << static_cast<int>(type)
            << " active=" << active_ << "\n";

        if (active_) {
            active_ = false;
            ++sessions_cancelled;

            std::osyncstream(std::cout)
                << "!!! Session " << session_id_ << " CANCELLING (total cancelled: "
                << sessions_cancelled.load() << ")\n";

            boost::system::error_code ec;
            socket_.cancel(ec);

            // Close the socket completely
            socket_.close(ec);

            std::osyncstream(std::cout)
                << "!!! Session " << session_id_ << " socket closed\n";
        }
    }

    void do_read()
    {
        if (!active_) return;

        auto self(shared_from_this());
        socket_.async_read_some(
            asio::buffer(data_),
            [this, self](boost::system::error_code ec, std::size_t length)
            {
                if (!ec && active_) {
                    // Process data...
                    std::osyncstream(std::cout)
                        << "Session " << this << " received " << length << " bytes\n";
                    do_read();
                } else if (ec) {
                    std::osyncstream(std::cout)
                        << "Session " << this << " error: " << ec.message() << "\n";
                }
            });
    }

    tcp::socket socket_;
    std::array<char, 1024> data_{};
    cancellation_signal& cancel_signal_;
    cancellation_slot slot_;
    bool active_{false};
    int session_id_;
};

class server
{
public:
    server(asio::any_io_executor executor, short port)
        : acceptor_(executor, tcp::endpoint(tcp::v4(), port))
        , signals_(executor)
        , global_cancel_signal_()
    {
        setup_signal_handling();
        do_accept();
    }

    void cancel_all_sessions()
    {
        std::osyncstream(std::cout)
            << "\n=== CANCELLING ALL SESSIONS ===\n"
            << "Total sessions created: " << sessions_created.load() << "\n"
            << "Sessions in vector: " << sessions_.size() << "\n"
            << "About to emit cancellation signal...\n";

        // Emit cancellation signal - this will trigger all connected slots
        global_cancel_signal_.emit(cancellation_type::all);

        std::osyncstream(std::cout)
            << "Cancellation signal emitted\n"
            << "Sessions cancelled so far: " << sessions_cancelled.load() << "\n";

        // Optional: Clear sessions list after some delay
        // Using a timer to allow graceful cleanup
        auto timer = std::make_shared<asio::steady_timer>(acceptor_.get_executor());
        timer->expires_after(std::chrono::seconds(2));
        timer->async_wait([this, timer](boost::system::error_code) {
            std::osyncstream(std::cout)
                << "\n=== CLEANUP TIMER FIRED ===\n"
                << "Sessions cancelled: " << sessions_cancelled.load() << "/"
                << sessions_created.load() << "\n"
                << "Sessions in vector: " << sessions_.size() << "\n"
                << "Clearing sessions vector...\n";
            sessions_.clear();
            std::osyncstream(std::cout)
                << "=== TEST COMPLETE ===\n\n";
        });
    }

private:
    void setup_signal_handling()
    {
        // Register for common termination signals
        signals_.add(SIGINT);
        signals_.add(SIGTERM);
#if defined(SIGQUIT)
        signals_.add(SIGQUIT);
#endif

        // Async wait for signals
        signals_.async_wait([this](boost::system::error_code ec, int signal_number) {
            handle_signal(ec, signal_number);
        });
    }

    void handle_signal(boost::system::error_code ec, int signal_number)
    {
        if (!ec) {
            std::osyncstream(std::cout)
                << "Received signal " << signal_number
                << ", initiating shutdown\n";

            // First, stop accepting new connections
            acceptor_.close();

            // Then cancel all existing sessions
            cancel_all_sessions();
        }
    }

    void do_accept()
    {
        acceptor_.async_accept(
            asio::bind_cancellation_slot(
                global_cancel_signal_.slot(),
                [this](boost::system::error_code ec, tcp::socket socket)
                {
                    if (!ec) {
                        std::osyncstream(std::cout)
                            << "New connection accepted\n";

                        // Create new session with reference to cancellation signal
                        auto new_session = std::make_shared<session>(
                            std::move(socket),
                            global_cancel_signal_
                        );
                        sessions_.push_back(new_session);
                        new_session->start();

                        // Continue accepting new connections
                        do_accept();
                    } else if (ec != asio::error::operation_aborted) {
                        std::osyncstream(std::cout)
                            << "Accept error: " << ec.message() << "\n";
                    }
                }));
    }

    tcp::acceptor acceptor_;
    asio::signal_set signals_;
    cancellation_signal global_cancel_signal_;
    std::vector<std::shared_ptr<session>> sessions_;
};

// Alternative: Session with its own cancellation signal
class session_with_local_cancel : public std::enable_shared_from_this<session_with_local_cancel>
{
public:
    session_with_local_cancel(tcp::socket socket, cancellation_signal& global_signal)
        : socket_(std::move(socket))
        , local_cancel_signal_()
    {
        // Connect local signal to global signal
        auto slot = global_signal.slot();
        slot.assign([this](cancellation_type type) {
            // Forward cancellation to local signal
            local_cancel_signal_.emit(type);
        });
    }

    void start()
    {
        do_read_with_local_cancellation();
    }

private:
    void do_read_with_local_cancellation()
    {
        auto self(shared_from_this());

        socket_.async_read_some(
            asio::buffer(data_),
            asio::bind_cancellation_slot(
                local_cancel_signal_.slot(),
                [this, self](boost::system::error_code ec, std::size_t length)
                {
                    if (ec == asio::error::operation_aborted) {
                        std::osyncstream(std::cout)
                            << "Read operation cancelled via cancellation slot\n";
                        return;
                    }

                    if (!ec) {
                        std::osyncstream(std::cout)
                            << "Read " << length << " bytes\n";
                        do_read_with_local_cancellation();
                    }
                }));
    }

    tcp::socket socket_;
    std::array<char, 1024> data_{};
    cancellation_signal local_cancel_signal_;
};

int main()
{
    asio::io_context io_context;

    // Create server with port 8080
    server s(io_context.get_executor(), 8080);

    std::cout << "\n========================================\n";
    std::cout << "Server started on port 8080\n";
    std::cout << "Creating 3 test client connections...\n";
    std::cout << "========================================\n\n";

    // Create 3 test clients
    auto create_test_clients = [&io_context, &s]() -> asio::awaitable<void> {
        auto executor = co_await asio::this_coro::executor;

        for (int i = 1; i <= 3; ++i) {
            try {
                tcp::socket socket(executor);
                co_await socket.async_connect(
                    tcp::endpoint(asio::ip::make_address("127.0.0.1"), 8080),
                    asio::use_awaitable);

                std::osyncstream(std::cout)
                    << "Test client " << i << " connected\n";

                // Keep socket alive by moving into a static vector
                static std::vector<tcp::socket> sockets;
                sockets.push_back(std::move(socket));
            } catch (const std::exception& e) {
                std::osyncstream(std::cout)
                    << "Test client " << i << " failed: " << e.what() << "\n";
            }
        }

        std::osyncstream(std::cout)
            << "\nAll test clients connected. Waiting 1 second...\n\n";

        // Wait a bit to let sessions stabilize
        asio::steady_timer timer(executor);
        timer.expires_after(std::chrono::seconds(1));
        co_await timer.async_wait(asio::use_awaitable);

        // Now trigger cancellation
        s.cancel_all_sessions();

        // Wait for cleanup
        timer.expires_after(std::chrono::seconds(3));
        co_await timer.async_wait(asio::use_awaitable);

        // Stop the io_context to exit
        io_context.stop();
    };

    asio::co_spawn(io_context, create_test_clients(), asio::detached);

    io_context.run();

    std::cout << "\n========================================\n";
    std::cout << "Server shutdown complete\n";
    std::cout << "FINAL RESULTS:\n";
    std::cout << "  Sessions created: " << sessions_created.load() << "\n";
    std::cout << "  Sessions cancelled: " << sessions_cancelled.load() << "\n";
    std::cout << "========================================\n\n";

    // Return success if all sessions were cancelled
    return (sessions_cancelled.load() == sessions_created.load()) ? 0 : 1;
}
